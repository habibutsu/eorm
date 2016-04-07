-module(eorm_builder_limit_offset).

-export([build_sql/1]).

build_sql(#{'query':=Query, expr:=Expr} = State) ->
    UpdExpr = build_offset(Query,
        build_limit(Query, Expr)),
    State#{expr => UpdExpr}.

build_limit(#{limit := Limit}, #{bindings:=Bindings, sql:=SQL} = Expr) ->
    Num = eorm_utils:to_binary(length(Bindings)+1),
    Expr#{
        sql => SQL ++ [<<"LIMIT $", Num/binary>>],
        bindings => Bindings ++ [Limit]};
build_limit(_Query, Expr) ->
    Expr.

build_offset(#{offset := Offset}, #{bindings:=Bindings, sql:=SQL} = Expr) ->
    Num = eorm_utils:to_binary(length(Bindings)+1),
    Expr#{
        sql => SQL ++ [<<"OFFSET $", Num/binary>>],
        bindings => Bindings ++ [Offset]};
build_offset(_Query, Expr) ->
    Expr.