-module(cberl_tests).
-include_lib("eunit/include/eunit.hrl").


cberl_test_() ->
    [{foreach, fun setup/0, fun clean_up/1,
      [fun test_set_and_get/1,
       fun test_replace_add/1,
       fun test_append_prepend/1]}].
%%%===================================================================
%%% Setup / Teardown
%%%===================================================================
setup() ->
    {ok, Instance} = cberl:new(),
    Instance.

clean_up(Instance) ->
    cberl:remove(Instance, "testkey"),
    cberl:remove(Instance, "testkey1"),
    ok = cberl:destroy(Instance).
%%%===================================================================
%%% Tests
%%%===================================================================

test_set_and_get(Instance) ->
    ok = cberl:set(Instance, "testkey", 0, "testval"),
    Get1 = cberl:get(Instance, "testkey"),
    ok = cberl:set(Instance, "testkey", 0, "testval", json),
    Get2 = cberl:get(Instance, "testkey"),
    ok = cberl:set(Instance, "testkey", 0, "testval", raw_binary),
    Get3 = cberl:get(Instance, "testkey"),
    ok = cberl:set(Instance, "testkey", 0, "testval", gzip),
    Get4 = cberl:get(Instance, "testkey"),
    [?_assertEqual({ok, 0, "testval"}, Get1),
     ?_assertEqual({ok, 0, "testval"}, Get2),
     ?_assertEqual({ok, 0, "testval"}, Get3),
     ?_assertEqual({ok, 0, "testval"}, Get4)
    ].

test_replace_add(Instance) ->
    ok = cberl:set(Instance, "testkey", 0, "testval"),
    AddFail = cberl:add(Instance, "testkey", 0, "testval"),
    AddPass = cberl:add(Instance, "testkey1", 0, "testval"),
    ReplaceFail = cberl:replace(Instance, "notestkey", 0, "testval"),
    ok = cberl:replace(Instance, "testkey", 0, "testval1"),
    Get1 = cberl:get(Instance, "testkey"),
    [?_assertEqual({error, key_eexists}, AddFail),
     ?_assertEqual(ok, AddPass),
     ?_assertEqual({error, key_enoent}, ReplaceFail),
     ?_assertEqual({ok, 0, "testval1"}, Get1)
    ].

test_append_prepend(Instance) ->
    ok = cberl:set(Instance, "testkey", 0, "base", str),
    ok = cberl:append(Instance, 0, "testkey", "tail"),
    Get1 = cberl:get(Instance, "testkey"),
    ok = cberl:prepend(Instance, 0, "testkey", "head"),
    Get2 = cberl:get(Instance, "testkey"),
    [?_assertEqual({ok, 0, "basetail"}, Get1),
     ?_assertEqual({ok, 0, "headbasetail"}, Get2)
    ]. 
%%%===================================================================
%%% Helper Functions
%%%===================================================================
