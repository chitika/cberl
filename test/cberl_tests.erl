-module(cberl_tests).
-include_lib("eunit/include/eunit.hrl").
-define(POOLNAME, testpool).

cberl_test_() ->
    [{foreach, fun setup/0, fun clean_up/1,
      [fun test_set_and_get/1,
       fun test_replace_add/1,
       fun test_append_prepend/1]}].
%%%===================================================================
%%% Setup / Teardown
%%%===================================================================
setup() ->
    cberl:start_link(?POOLNAME, 3),
    ok.

clean_up(_) ->
    cberl:remove(?POOLNAME, "testkey"),
    cberl:remove(?POOLNAME, "testkey1"),
    cberl:remove(?POOLNAME, "notestkey"),
    cberl:stop(?POOLNAME).
%%%===================================================================
%%% Tests
%%%===================================================================

test_set_and_get(_) ->
    ok = cberl:set(?POOLNAME, "testkey", 0, "testval"),
    Get1 = cberl:get(?POOLNAME, "testkey"),
    ok = cberl:set(?POOLNAME, "testkey", 0, "testval", json),
    Get2 = cberl:get(?POOLNAME, "testkey"),
    ok = cberl:set(?POOLNAME, "testkey", 0, "testval", raw_binary),
    Get3 = cberl:get(?POOLNAME, "testkey"),
    [?_assertEqual({"testkey", 0, "testval"}, Get1),
     ?_assertEqual({"testkey", 0, "testval"}, Get2),
     ?_assertEqual({"testkey", 0, "testval"}, Get3)
    ].

test_replace_add(_) ->
    ok = cberl:set(?POOLNAME, "testkey", 0, "testval"),
    AddFail = cberl:add(?POOLNAME, "testkey", 0, "testval"),
    AddPass = cberl:add(?POOLNAME, "testkey1", 0, "testval"),
    ReplaceFail = cberl:replace(?POOLNAME, "notestkey", 0, "testval"),
    ok = cberl:replace(?POOLNAME, "testkey", 0, "testval1"),
    Get1 = cberl:get(?POOLNAME, "testkey"),
    [?_assertEqual({error, key_eexists}, AddFail),
     ?_assertEqual(ok, AddPass),
     ?_assertEqual({error, key_enoent}, ReplaceFail),
     ?_assertEqual({"testkey", 0, "testval1"}, Get1)
    ].

test_append_prepend(_) ->
    ok = cberl:set(?POOLNAME, "testkey", 0, "base", str),
    ok = cberl:append(?POOLNAME, 0, "testkey", "tail"),
    Get1 = cberl:get(?POOLNAME, "testkey"),
    ok = cberl:prepend(?POOLNAME, 0, "testkey", "head"),
    Get2 = cberl:get(?POOLNAME, "testkey"),
    [?_assertEqual({"testkey", 0, "basetail"}, Get1),
     ?_assertEqual({"testkey", 0, "headbasetail"}, Get2)
    ]. 
%%%===================================================================
%%% Helper Functions
%%%===================================================================
