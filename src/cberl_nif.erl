-module(cberl_nif).
-export([new/0, control/3, destroy/1]).

-on_load(init/0).

-define(NIF_STUB, exit(nif_library_not_loaded)).

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
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
