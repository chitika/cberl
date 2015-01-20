-module(cberl_nif).
-export([new/0, control/3, destroy/1]).

-on_load(init/0).

-define(NIF_STUB, erlang:nif_error(nif_library_not_loaded)).

init() ->
    PrivDir = case code:priv_dir(cberl) of
                  {error, bad_name} ->
                      re:replace(code:which(?MODULE), "cberl.*", "cberl/priv",[{return,list}]);
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, "cberl_drv"), 0).

new() ->
    ?NIF_STUB.

control(_, _, _) ->
    ?NIF_STUB.

destroy(_) ->
    ?NIF_STUB.
