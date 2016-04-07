-module(eorm_builder_insert).

-compile([export_all]).

% http://www.postgresql.org/docs/9.4/static/sql-insert.html
% INSERT INTO table_name [ ( column_name [, ...] ) ]
%     { DEFAULT VALUES | VALUES ( { expression | DEFAULT } [, ...] ) [, ...] | query }
%     [ RETURNING * | output_expression [ [ AS ] output_name ] [, ...] ]

new_expr() ->
    #{
        values => []
        ,fields => []
        ,bindings => []
        ,returning => []
        ,sql => [<<"insert into">>]
    }.

build(Entity, Obj, InQuery) ->
    #{fields := DbFields} = Entity,
    Query = eorm:cb_prepare_obj_statement(Entity, Obj,
        {insert, InQuery#{values => eorm_object:attrs_with(DbFields, Obj)}}),
    Table = eorm:get_table(Entity, {insert, Query}),
    State = #{
        entity => Entity,
        'query' => Query#{table => Table},
        expr => new_expr()
    },
    erlz:do(State, [
        fun build_table/1
        ,fun build_values/1
        ,fun eorm_builder_returning:build_sql/1
        ,fun eorm_builder:build_sql/1
    ]).

build_table(#{'query' := #{table := Table}, expr := #{sql := SQL} = Expr} = State) ->
    State#{expr => Expr#{sql => SQL ++ [Table]}}.

build_values(#{'query' := Query, expr:=Expr} = State) ->
    #{sql := SQL} = Expr,
    #{values := Values} = Query,
    {Columns, Bindings} = lists:unzip(maps:to_list(Values)),

    BinValues = eorm_utils:binary_join(
        lists:map(
            fun(V) ->
                Num = erlang:integer_to_binary(V),
                <<"$", Num/binary>>
            end,
            lists:seq(1, length(Bindings))),
        <<",">>),
    BinColumns = eorm_utils:binary_join(
        lists:map(
            fun eorm_utils:to_binary/1, Columns),
        <<",">>),
    State#{expr => Expr#{
        bindings => Bindings,
        sql => SQL ++ [
            <<"(", BinColumns/binary, ")">>,
            <<"values (", BinValues/binary, ")">>
        ]
    }}.
