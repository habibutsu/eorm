-module(eorm_builder_order_by).

-export([build_sql/1]).

column_to_sql({Column, Order}) ->
    BinColumn = eorm_utils:to_binary(Column),
    BinOrder = eorm_utils:to_binary(Order),
    <<BinColumn/binary, " ", BinOrder/binary>>;
column_to_sql(Column) ->
    eorm_utils:to_binary(Column).

build_sql(#{'query' := #{order_by := Columns}, expr := #{sql := SQL} = Expr} = State) ->
    OrderBy = case is_list(Columns) of
        true -> eorm_utils:binary_join(
            lists:map(fun column_to_sql/1, Columns), <<",">>);
        _ -> column_to_sql(Columns)
    end,
    State#{expr => Expr#{sql => SQL ++ [<<"ORDER BY ", OrderBy/binary>>]}};
build_sql(State) ->
    State.
