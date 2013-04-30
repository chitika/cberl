-module(cberl_tests).
-include_lib("eunit/include/eunit.hrl").
-define(POOLNAME, testpool).

cberl_test_() ->
    [{foreach, fun setup/0, fun clean_up/1,
      [fun test_set_and_get/1,
       fun test_replace_add/1,
       fun test_get_and_touch/1,
       fun test_append_prepend/1,
       fun test_remove/1]}].

%%%===================================================================
%%% Setup / Teardown
%%%===================================================================

setup() ->
    cberl:start_link(?POOLNAME, 3),
    ok.

clean_up(_) ->
    cberl:remove(?POOLNAME, <<"testkey">>),
    cberl:remove(?POOLNAME, <<"testkey1">>),
    cberl:remove(?POOLNAME, <<"notestkey">>),
    cberl:stop(?POOLNAME).

%%%===================================================================
%%% Tests
%%%===================================================================

test_set_and_get(_) ->
    Key = <<"testkey">>,
    Value = "testval",
    ok = cberl:set(?POOLNAME, Key, 0, Value),
    Get1 = cberl:get(?POOLNAME, Key),
    ok = cberl:set(?POOLNAME, Key, 0, Value, json),
    Get2 = cberl:get(?POOLNAME, Key),
    ok = cberl:set(?POOLNAME, Key, 0, Value, raw_binary),
    Get3 = cberl:get(?POOLNAME, Key),
    [?_assertMatch({Key, _, Value}, Get1),
     ?_assertMatch({Key, _, Value}, Get2),
     ?_assertMatch({Key, _, Value}, Get3)
    ].

test_replace_add(_) ->
    Key = <<"testkey">>,
    Value = "testval",
    ok = cberl:set(?POOLNAME, Key, 0, Value),
    AddFail = cberl:add(?POOLNAME, Key, 0, Value),
    AddPass = cberl:add(?POOLNAME, <<"testkey1">>, 0, Value),
    ReplaceFail = cberl:replace(?POOLNAME, <<"notestkey">>, 0, Value),
    ok = cberl:replace(?POOLNAME, Key, 0, "testval1"),
    Get1 = cberl:get(?POOLNAME, Key),
    [?_assertEqual({error, key_eexists}, AddFail),
     ?_assertEqual(ok, AddPass),
     ?_assertEqual({error, key_enoent}, ReplaceFail),
     ?_assertMatch({Key, _, "testval1"}, Get1)
    ].

test_append_prepend(_) ->
    Key = <<"testkey">>,
    ok = cberl:set(?POOLNAME, Key, 0, "base", str),
    ok = cberl:append(?POOLNAME, 0, Key, "tail"),
    Get1 = cberl:get(?POOLNAME, Key),
    ok = cberl:prepend(?POOLNAME, 0, Key, "head"),
    Get2 = cberl:get(?POOLNAME, Key),
    [?_assertMatch({Key, _, "basetail"}, Get1),
     ?_assertMatch({Key, _, "headbasetail"}, Get2)
    ]. 

test_get_and_touch(_) ->
    Key = <<"testkey">>,
    Value = "testval",
    ok = cberl:set(?POOLNAME, Key, 0, Value),
    cberl:get_and_touch(?POOLNAME, Key, 1),
    timer:sleep(5000),
    [?_assertEqual({Key, {error,key_enoent}}, cberl:get(?POOLNAME, Key))].

test_remove(_) ->
    Key = <<"testkey">>,
    Value = "testval",
    ok = cberl:set(?POOLNAME, Key, 0, Value),
    ok = cberl:remove(?POOLNAME, Key),
    [?_assertEqual({Key, {error,key_enoent}}, cberl:get(?POOLNAME, Key))].
