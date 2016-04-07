-module(eorm_builder_returning).

-export([build_sql/1]).

build_sql(#{'query' := #{returning := Columns}, expr := #{sql := SQL} = Expr} = State) ->
    BinColumns = eorm_utils:binary_join(
            lists:map(fun eorm_utils:to_binary/1, Columns), <<",">>),
    State#{expr => Expr#{sql => SQL ++ [<<"returning ", BinColumns/binary>>]}};
build_sql(#{expr := #{sql := SQL} = Expr} = State) ->
    State#{expr => Expr#{sql => SQL ++ [<<"returning id">>]}}.
