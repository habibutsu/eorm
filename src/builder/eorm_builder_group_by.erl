-module(eorm_builder_group_by).

-export([build_sql/1]).

build_sql(#{
        'query' := #{
            table := Table,
            group_by := Columns}, expr := #{sql := SQL} = Expr} = State) ->
    GroupBy = case is_list(Columns) of
        true ->
            eorm_utils:binary_join(
                lists:map(fun(Column) ->
                        <<Table/binary, ".", (eorm_utils:to_binary(Column))/binary>>
                    end),
                    <<",">>);
        _ ->
            <<Table/binary, ".", (eorm_utils:to_binary(Columns))/binary>>
    end,
    State#{expr => Expr#{sql => SQL ++ [<<"GROUP BY ", GroupBy/binary>>]}};
build_sql(State) ->
    State.
