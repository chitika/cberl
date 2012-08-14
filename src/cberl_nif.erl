-module(cberl_nif).

-export([new/4,
         store/7,
         mget/3,
         getl/3,
         unlock/3,
         mtouch/3,
         arithmetic/6,
         remove/3,
         destroy/1]).

-on_load(init/0).

-define(nif_stub, nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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


new(_,_,_,_) ->
    ?nif_stub.

store(_Instance, _Op, _Key, _Value, _Flags, _Exp, _Cas) ->
    ?nif_stub.

mget(_Instance, _Key, _Exp) ->
    ?nif_stub.

getl(_Instance, _Key, _Exp) ->
    ?nif_stub.

unlock(_Instance, _Key, _Cas) ->
    ?nif_stub.

mtouch(_Instance, _Key, _Exp) ->
    ?nif_stub.

arithmetic(_Instance, _Key, _Delta, _Exp, _Create, _Initial) ->
    ?nif_stub.

remove(_Instance, _Key, _Cas) ->
    ?nif_stub.

destroy(_Instance) ->
    ?nif_stub.
