-module(eorm_update_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile([export_all]).

all() -> [
    update_test
].

init_per_suite(Config) ->
    test_utils:init_per_suite(Config).

end_per_suite(Config) ->
    test_utils:end_per_suite(Config).


update_test(_Config) ->
    Obj = eorm_object:new(post, #{
        <<"id">> => 1,
        <<"user_id">> => 1,
        <<"data">> => <<"test ", (integer_to_binary(crypto:rand_uniform(0, 100)))/binary>>
    }),
    SQL = eorm_db:update(Obj, #{
        as_sql => true,
        fields => [data]
    }),
    ct:log("SQL ~p", [SQL]),

    {ok, UpdObj} = eorm_db:update(Obj, #{
        fields => [data],
        returning => [created_at]
    }),
    ct:log("UpdObj: ~p", [UpdObj]),

    {ok, [SelectedObj]} = eorm_db:select(post, #{
        where => #{
            id => 1,
            user_id => 1
        }
    }),
    ct:log("SelectedObj ~p", [SelectedObj]),
    ok.

