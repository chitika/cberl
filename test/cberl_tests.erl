-module(cberl_tests).
-include_lib("eunit/include/eunit.hrl").


cberl_test_() ->
    [{foreach, fun setup/0, fun clean_up/1,
      [fun test_basics/1]}].
%%%===================================================================
%%% Setup / Teardown
%%%===================================================================
setup() ->
    {ok, Instance} = cberl:new(),
    Instance.

clean_up(Instance) ->
     ok = cberl:destroy(Instance).
%%%===================================================================
%%% Tests
%%%===================================================================

test_basics(Instance) ->
    ok = cberl:set(Instance, "cberlskey", "cberlstringval"),   
    ok = cberl:set(Instance, "cberlikey", 9631),   
    ok = cberl:set(Instance, "cberlbkey", <<"binary">>),   
    GetS = cberl:mget(Instance, "cberlskey"),
    GetI = cberl:mget(Instance, "cberlikey"),
    GetB = cberl:mget(Instance, "cberlbkey"),
    AddFail = cberl:add(Instance, "cberlskey", "dummy"),
    ok = cberl:add(Instance, "addpass", "hooray"),
    AddPass = cberl:mget(Instance, "addpass"),
    RepFail = cberl:replace(Instance, "nonexistingkey", "failed"),
    ok = cberl:replace(Instance, "cberlskey", "replaced"),
    CheckAdd = cberl:mget(Instance, "addpass"),
    CheckRep = cberl:mget(Instance, "cberlskey"),
    ok =  cberl:append(Instance, "cberlskey", "tail"),
    ok =  cberl:prepend(Instance, "cberlskey", "head"),
    CheckPend = cberl:mget(Instance, "cberlskey"),
    ok = cberl:remove(Instance, "cberlskey"),
    ok = cberl:remove(Instance, "cberlikey"),
    ok = cberl:remove(Instance, "cberlbkey"),
    ok = cberl:remove(Instance, "addpass"),
    CheckRemove = cberl:mget(Instance, "addpass"),
    [?_assertEqual({ok, "cberlstringval"}, GetS),
     ?_assertEqual({ok, 9631}, GetI),
     ?_assertEqual({ok, <<"binary">>}, GetB),
     ?_assertEqual({ok, "hooray"}, AddPass),
     ?_assertEqual({error, key_eexists}, AddFail),
     ?_assertEqual({error, key_enoent}, RepFail),
     ?_assertEqual({ok, "hooray"}, CheckAdd),
     ?_assertEqual({ok, "replaced"}, CheckRep),
     ?_assertEqual({ok, "headreplacedtail"}, CheckPend),
     ?_assertEqual({error, key_enoent}, CheckRemove)
    ].
%%%===================================================================
%%% Helper Functions
%%%===================================================================
