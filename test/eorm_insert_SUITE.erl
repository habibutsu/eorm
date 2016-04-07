-module(eorm_insert_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile([export_all]).

all() -> [
    insert_test
].

init_per_suite(Config) ->
    test_utils:init_per_suite(Config).

end_per_suite(Config) ->
    test_utils:end_per_suite(Config).


insert_test(_Config) ->
    Obj = eorm_object:new(post, #{
        <<"user_id">> => 1,
        <<"data">> => <<"test">>
    }),
    SQL = eorm_db:insert(Obj, #{as_sql => true}),
    ct:log("SQL: ~p", [SQL]),

    {ok, UpdObj} = eorm_db:insert(Obj),
    ct:log("Obj: ~p", [UpdObj]),

    11 = eorm_object:id(UpdObj),
    ok.
