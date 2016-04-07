-module(eorm_builder_select).

-compile([export_all]).

% http://www.postgresql.org/docs/9.4/static/sql-select.html
% SELECT [ ALL | DISTINCT [ ON ( expression [, ...] ) ] ]
%     [ * | expression [ [ AS ] output_name ] [, ...] ]
%     [ FROM from_item [, ...] ]
%     [ WHERE condition ]
%     [ GROUP BY expression [, ...] ]
%     [ HAVING condition [, ...] ]
%     [ WINDOW window_name AS ( window_definition ) [, ...] ]
%     [ { UNION | INTERSECT | EXCEPT } [ ALL | DISTINCT ] select ]
%     [ ORDER BY expression [ ASC | DESC | USING operator ] [ NULLS { FIRST | LAST } ] [, ...] ]
%     [ LIMIT { count | ALL } ]
%     [ OFFSET start [ ROW | ROWS ] ]
%     [ FETCH { FIRST | NEXT } [ count ] { ROW | ROWS } ONLY ]
%     [ FOR { UPDATE | NO KEY UPDATE | SHARE | KEY SHARE } [ OF table_name [, ...] ] [ NOWAIT ] [...] ]

new_expr() ->
    #{
        fields => []
        ,joins => []
        ,where => []
        ,bindings => []
        ,extra_query => []
        ,sql => [<<"select">>]
    }.

build(Entity, Query) ->
    build(Entity, Query, new_expr()).

build(Entity, InQuery, Expr) ->
    Query = eorm:cb_prepare_statement(Entity, {select, InQuery}),
    erlz:do(
        build_expr(Entity, Query, Expr), [
        fun build_fields_sql/1
        ,fun build_from_sql/1
        ,fun eorm_builder_with:build_sql/1
        ,fun eorm_builder_where:build_sql/1
        ,fun eorm_builder_group_by:build_sql/1
        ,fun eorm_builder_order_by:build_sql/1
        ,fun eorm_builder_limit_offset:build_sql/1
        ,fun eorm_builder:build_sql/1
    ]).

build_expr(Entity, Query, Expr) ->
    State = #{
        entity => Entity,
        'query' => Query,
        expr => Expr
    },
    build_expr(State).

build_expr(State) ->
    erlz:do(State,[
        % build expr
        fun get_table/1
        ,fun build_fields/1
        ,fun eorm_builder_where:build/1
        ,fun eorm_builder_with:build/1
    ]).

get_table(#{entity := Entity, 'query' := Query} = State) ->
    Table = eorm:get_table(Entity, {select, Query}),
    State#{'query' => Query#{table => Table}}.

build_from_sql(#{'query' := #{table := Table}, expr := #{sql := SQL} = Expr} = State) ->
    State#{expr => Expr#{sql => SQL ++ [<<"from ", Table/binary>>]}}.

build_field(_Table, {sql, Value}) ->
    Value;
build_field(Table, Value) ->
    F = eorm_utils:to_binary(Value),
     <<Table/binary, ".", F/binary>>.

build_fields(#{entity :=Entity, 'query' := Query, expr := Expr} = State) ->
    #{table := Table} = Query,
    #{type := Type, fields := DefFields} = Entity,
    #{fields := ExprFields} = Expr,
    DbFields = maps:get(fields, Query, DefFields),
    UpdExprFields = ExprFields ++ [{{Type, Table}, DbFields}],
    State#{expr => Expr#{fields => UpdExprFields}}.

build_fields_sql(#{expr := #{fields:=Fields, sql := Sql} = Expr} = State) ->
    SQLFields = eorm_utils:binary_join(
        lists:foldl(
            fun({{_Type, Table}, Fs}, Acc) ->
                Acc ++ lists:map(
                    erlz:partial(fun build_field/2, [Table]),
                    Fs)
            end,
            [],
            Fields), <<",">>),
    State#{expr => Expr#{sql => Sql ++ [SQLFields]}}.
