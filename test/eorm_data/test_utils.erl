-module(test_utils).

-include_lib("common_test/include/ct.hrl").

-compile([export_all]).

init_per_suite(Config) ->
    application:ensure_all_started(eorm),
    application:ensure_all_started(epgsql),
    init_db(),
    SelfPid = self(),
    Pid = spawn(fun() ->
        try
            test_entities:init()
        of
            ok ->
                SelfPid ! ok,
                receive
                    stop -> exit(normal)
                end
        catch
            Class:Error ->
                ct:log("Stacktrace: ~p", [erlang:get_stacktrace()]),
                SelfPid ! {error, {Class, Error}},
                exit(normal)
        end
    end),
    receive
        ok -> ok;
        {error, Reason} ->
            ct:fail({error, Reason})
    end,
    Config ++ [{eorm_owner, Pid}].

end_per_suite(Config) ->
    ?config(eorm_owner, Config) ! stop,
    clean_db(),
    ok.

new_connection() ->
    epgsql:connect(
        "127.0.0.1", "dbuser", "dbpassword", [
            {database, "testdb"},
            {timeout, 4000}
    ]).

init_db() ->
    {ok, Conn} = new_connection(),
    {ok, SQLSchema} = file:read_file(
        filename:join([code:lib_dir(eorm), "test", "eorm_data", "schema.sql"])),

    Result = erlz:error_traverse(
        fun(Query) ->
            case
                epgsql:equery(Conn, Query, [])
            of
                {error, Error} -> {error, Error};
                _ -> {ok, ok}
            end
        end,
        binary:split(SQLSchema, <<";\n">>, [global])),
    case Result of
        {error, Error} ->
            clean_db(),
            ct:fail("Init db fail ~p", [Error]);
        _ -> ok
    end.

clean_db() ->
    {ok, Conn} = new_connection(),
    Result = epgsql:squery(Conn, "drop owned by dbuser cascade"),
    ok.
