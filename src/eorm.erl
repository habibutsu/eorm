-module(eorm).

-export([
    init/0
    ,destroy/0
    ,def_db/1
    ,cb_prepare_statement/2
    ,cb_prepare_obj_statement/3
    ,get_table/2
    ,get_connection/2
    ,def_entity/2
    ,get_entity/1
    ,transform_to/2
    ,transform_from/2

]).

init() ->
    ?MODULE = ets:new(?MODULE, [
        named_table,
        public,
        {read_concurrency, true}
    ]).

destroy() ->
    ets:delete(?MODULE).

def_db(_DbDeclaration) ->
    throw({ton_implemented}).

cb_prepare_statement(
        #{callbacks := #{prepare_statement:=Fn}} = _Entity, Query) ->
    Fn(Query);
cb_prepare_statement(_Entity, {_Kind, Query}) ->
    Query.

cb_prepare_obj_statement(
        #{callbacks := #{prepare_obj_statement:=Fn}} = _Entity, Obj, Query) ->
    Fn(Obj, Query);
cb_prepare_obj_statement(_Entity, _Obj, {_Kind, Query}) ->
    Query.

prepare_relationship(Entity) ->
    lists:foldl(
        fun(Kind, Acc) ->
            Types = lists:map(fun eorm_utils:to_binary/1,
                maps:get(Kind, Entity, [])),
            Length = length(Types),
            maps:merge(
                Acc,
                maps:from_list(
                    lists:zip(
                        Types,
                        lists:duplicate(Length, Kind))))
        end,
        #{},
        ['has-many', 'has-one', 'belongs-to']).

get_table(#{table:=GetFn} = _Entity, Query) when is_function(GetFn) ->
    GetFn(Query);
get_table(#{table:=Table} = _Entity, _Query) ->
    eorm_utils:to_binary(Table).


get_connection(#{
        db_connection:=GetFn,
        adapter:=Adapter} = _Entity, Query) when is_function(GetFn) ->
    {Adapter, GetFn(Query)};
get_connection(#{
        db_connection:=Connection,
        adapter:=Adapter} = _Entity, _Query) ->
    {Adapter, Connection};
get_connection(_Entity, _Query) ->
    throw({bad_entity, no_connection}).

reflect_table(#{fields := Fields} = Entity) ->
    Entity#{
        fields => lists:map(fun eorm_utils:to_binary/1, Fields)
    };
reflect_table(Entity) ->
    ReflectQuery = {reflect, #{}},
    Table = get_table(Entity, ReflectQuery),
    Connection = get_connection(Entity, ReflectQuery),
    case
        eorm_db:exec_query(Connection, <<
            "select array(select column_name::text from information_schema.columns ",
            "where table_schema=current_schema() and table_name = $1) as columns">>, [Table])
    of
        {ok, []} ->
            throw({error_reflect, {"Could not get columns from table ", Table}});
        {ok, [Row]} ->
            Entity#{
                fields => proplists:get_value(<<"columns">>, Row)};
        {error, Reason} ->
            throw({error_reflect, Reason})
    end.

def_entity(InType, Entity) ->
    Type = eorm_utils:to_binary(InType),
    Adapter = maps:get(adapter, Entity, adapter_epgsql),
    % normalization
    UpdEntity = reflect_table(Entity#{
        adapter => Adapter,
        relationship => prepare_relationship(Entity),
        type => Type,
        pk => eorm_utils:to_binary(maps:get(pk, Entity, <<"id">>))
    }),
    true = ets:insert(?MODULE, {{type, Type}, UpdEntity}).

get_entity(Type) when is_atom(Type) ->
    get_entity(atom_to_binary(Type, utf8));

get_entity(Type) ->
    case ets:lookup(?MODULE, {type, Type}) of
        [{{type, Type}, Entity}] -> Entity;
        _ ->
            throw({entity_not_exists, Type})
    end.



transform_to(Format, Obj) ->
    transform('transform-to', Format, Obj).


transform_from(Format, Obj) ->
    transform('transform-from', Format, Obj).


transform(TransformKey, Format, #{type := Type} = Obj) ->
    Entity = get_entity(Type),
    transform(TransformKey, Format, Obj, Entity).

transform(TransformKey, Format, Obj, Entity) ->
    #{attributes := Attrs, linked := Linked} = Obj,
    case Entity of
        #{TransformKey := #{Format := Transforms}} ->
            UpdAttrs = transform_attrs(Transforms, Attrs),
            UpdLinked = maps:map(
                fun(_K, V) ->
                    lists:map(fun(LObj) -> transform(TransformKey, Format, LObj) end, V)
                end,
                Linked),
            Obj#{attributes => UpdAttrs, linked := UpdLinked};
        _ ->
            Obj
    end.

transform_attrs(Transforms, Attrs) when is_list(Transforms) ->
    lists:foldl(
        fun(Transform, AttrsIn) -> transform_attrs(Transform, AttrsIn) end,
        Attrs,
        Transforms);

transform_attrs(Transform, Attrs) when is_function(Transform)->
    Transform(Attrs);

transform_attrs(Transform, Attrs) when is_map(Transform)->
    maps:fold(
        fun(Key, Fn, AttrsIn) ->
            case maps:get(Key, AttrsIn, undefined) of
                undefined -> AttrsIn;
                V -> AttrsIn#{Key => Fn(V)}
            end
        end,
        Attrs, Transform).