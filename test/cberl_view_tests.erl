-module(cberl_view_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("couchbase_connection.hrl").

cberl_view_test_() ->
    [{foreach, fun setup/0, fun clean_up/1,
      [fun test_set_design_doc/1,
       fun test_remove_design_doc/1,
       fun test_query_view/1]}].


%%%===================================================================
%%% Setup / Teardown
%%%===================================================================

setup() ->
    cberl:start_link(?POOLNAME, 3,
                     ?COUCHBASE_HOST,
                     ?COUCHBASE_USER,
                     ?COUCHBASE_PASSWORD),
    cberl:set_design_doc(?POOLNAME, "test-design-doc",
                         {[{<<"views">>,
                            {[{<<"test-view">>,
                               {[{<<"map">>, <<"function(doc,meta){}">>}]}
                              }]}
                           }]}),
    ok.

clean_up(_) ->
    cberl:stop(?POOLNAME).

%%%===================================================================
%%% Tests
%%%===================================================================
test_query_view(_) ->
    DocName = "test-set-design-doc",
    ViewName = "test-view",
    [?_assertMatch({ok, {0, []}}, cberl:view(?POOLNAME, DocName, ViewName, []))].

test_set_design_doc(_) ->
    DocName = "test-set-design-doc",
    ViewName = "test-view",
    DesignDoc = {[{<<"views">>,
                   {[{list_to_binary(ViewName),
                      {[{<<"map">>, <<"function(doc,meta){}">>}]}
                     }]}
                  }]},
    fun () ->
        [?assertEqual(ok, cberl:set_design_doc(?POOLNAME, DocName, DesignDoc)),
         ?assertMatch({ok, _}, cberl:view(?POOLNAME, DocName, ViewName, []))]
    end.

test_remove_design_doc(_) ->
    DocName = "test-design-doc",
    ViewName = "test-view",
    fun () ->
        [?assertEqual(ok, cberl:remove_design_doc(?POOLNAME, DocName)),
         ?assertMatch({error, _}, cberl:view(?POOLNAME, DocName, ViewName, []))]
    end.
