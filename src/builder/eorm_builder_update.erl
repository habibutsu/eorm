-module(eorm_builder_update).

-compile([export_all]).

% http://www.postgresql.org/docs/9.4/static/sql-update.html

% UPDATE [ ONLY ] table_name [ * ] [ [ AS ] alias ]
%     SET { column_name = { expression | DEFAULT } |
%           ( column_name [, ...] ) = ( { expression | DEFAULT } [, ...] ) } [, ...]
%     [ FROM from_list ]
%     [ WHERE condition | WHERE CURRENT OF cursor_name ]
%     [ RETURNING * | output_expression [ [ AS ] output_name ] [, ...] ]

new_expr() ->
    #{
        bindings => []
        ,fields => []
        ,where => []
        ,returning => []
        ,sql => [<<"update">>]
    }.


build(Entity, Obj, Query) ->
    State = #{
        entity => Entity,
        obj => Obj,
        'query' => Query,
        expr => new_expr()
    },
    erlz:do(State, [
        fun update_query_where/1
        ,fun build_table/1
        ,fun prepare_values/1
        ,fun build_set/1
        ,fun eorm_builder_where:build/1
        ,fun eorm_builder_where:build_sql/1
        ,fun eorm_builder_returning:build_sql/1
        ,fun eorm_builder:build_sql/1
    ]).

update_query_where(#{'query' := Query, obj := Obj} = State) ->
    ID = eorm_object:id(Obj),
    Where = maps:get(where, Query, #{}),
    State#{'query'=> Query#{where => Where#{id => ID}}}.

build_table(#{entity := Entity, 'query' := InQuery, obj := Obj} = State) ->
    Query = eorm:cb_prepare_obj_statement(Entity, Obj, {update, InQuery}),
    Table = eorm:get_table(Entity, {update, Query}),
    #{expr := #{sql := SQL} = Expr} = State,
    State#{
        'query' => Query#{table => Table},
        expr => Expr#{sql => SQL ++ [Table]}
    }.

prepare_values(#{entity := Entity, 'query' := Query, obj := Obj} = State) ->
    DbFields = case Query of
        #{fields := F} -> lists:map(fun eorm_utils:to_binary/1, F);
        _ -> maps:get(fields, Entity)
    end,
    State#{'query' => Query#{values => eorm_object:attrs_with(DbFields, Obj)}}.


build_set(#{'query' := #{values := Values}, expr := Expr} = State) ->
    {Columns, Bindings} = lists:unzip(maps:to_list(Values)),
    #{sql := SQL} = Expr,

    {SetItems, _} = lists:mapfoldl(
        fun(F, Num) ->
            BinNum = erlang:integer_to_binary(Num),
            {<<F/binary, " = $", BinNum/binary>>, Num+1}
        end,
        1,
        Columns),
    Set = eorm_utils:binary_join(SetItems, <<",">>),

    State#{expr => Expr#{
        bindings => Bindings,
        sql => SQL ++ [<<"set ", Set/binary>>]
    }}.