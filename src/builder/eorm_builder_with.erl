-module(eorm_builder_with).

-export([
    build/1
    ,build_sql/1
]).


build(#{'query' := #{with := []}} = State) ->
    State;

build(#{
        entity := FromEntity,
        'query' := #{with := InWith, table := FromTable},
        expr := InExpr} = State) ->
    UpdExpr = lists:foldl(
        fun(WithItem, Expr) ->
            build_with(WithItem, FromEntity, FromTable, Expr)
        end,
        InExpr,
        InWith),
    State#{expr => UpdExpr};

build(State) ->
    State.

build_sql(#{expr:=#{joins := []}} = State) ->
    State;
build_sql(#{expr:=#{sql := SQL, joins := Joins} = Expr} = State) ->
    State#{expr => Expr#{sql => SQL ++ Joins}}.



build_with({InToType, Query}, FromEntity, FromTable, Expr) ->
    #{relationships := Relationships} = FromEntity,
    ToType = eorm_utils:to_binary(InToType),
    Relation = maps:get(ToType, Relationships, undefined),
    build_relation(Relation, {ToType, Query}, FromEntity, FromTable, Expr);

build_with(InToType, FromEntity, FromTable, Expr) ->
    build_with({InToType, #{}}, FromEntity, FromTable, Expr).

build_relation({'belongs-to', RelationKey}, {ToType, Query}, _FromEntity, FromTable, Expr) ->
    ToEntity = eorm:get_entity(ToType),
    #{pk := ToPk} = ToEntity,

    #{
        'query' := #{table := ToTable},
        expr := UpdExpr
    } = eorm_builder_select:build_expr(ToEntity, Query, Expr),

    Join = <<"left join ",
        ToTable/binary, " on ",
        FromTable/binary, ".", RelationKey/binary, " = ",
        ToTable/binary, ".", ToPk/binary>>,

    #{joins := Joins} = UpdExpr,
    UpdExpr#{joins => Joins ++ [Join]};


build_relation({'has-one', RelationKey}, {ToType, Query}, FromEntity, FromTable, Expr) ->
    #{
        type := FromType,
        pk := FromPk
    } = FromEntity,
    ToEntity = eorm:get_entity(ToType),
    #{
        'query' := #{table := ToTable},
        expr := UpdExpr
    } = eorm_builder_select:build_expr(ToEntity, Query, Expr),

    Join = <<"left join ",
        ToTable/binary, " on ",
        FromTable/binary, ".", FromPk/binary, " = ",
        ToTable/binary, ".", RelationKey/binary>>,

    #{joins := Joins} = UpdExpr,
    UpdExpr#{joins => Joins ++ [Join]};

build_relation({'has-many', RelationKey}, {ToType, Query}, _FromEntity, _FromTable, #{extra_query := ExtraQuery} = Expr) ->
    Expr#{extra_query => ExtraQuery ++ [{ToType, RelationKey, Query}]};

build_relation(undefined, {ToType, _Query}, #{type:=FromType} = _FromEntity, _FromTable, _Expr) ->
    throw({no_relationship, {FromType, ToType}}).