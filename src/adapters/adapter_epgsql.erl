-module(adapter_epgsql).

-export([
    exec_query/3
]).

-include_lib("epgsql/include/epgsql.hrl").

rows_to_proplist(Cols, Rows) ->
    lists:map(
        fun(Row) ->
            lists:zipwith(
                fun(#column{name=K} = C, V) -> {K, V} end,
                Cols, tuple_to_list(Row))
        end,
        Rows).

exec_query(Conn, Query, Bindings) ->
    case
        epgsql:equery(Conn, Query, Bindings)
    of
        {ok, Count, Cols, Rows} ->
            {ok, {Count, rows_to_proplist(Cols, Rows)}};
        {ok, _Cols, []} ->
            {ok, []};
        {ok, Cols, Rows} ->
            {ok, rows_to_proplist(Cols, Rows)};
        {ok, Count} ->
            {ok, {Count, []}};
        {error, Error} ->
            {error, Error}
    end.