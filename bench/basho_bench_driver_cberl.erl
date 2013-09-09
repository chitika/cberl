-module(basho_bench_driver_cberl).

-export([new/1, 
         run/4
         ]).

-include("basho_bench.hrl").

new(_Id) ->
    PoolSize = basho_bench_config:get(cberl_pool_size, 5),
    HostPort = basho_bench_config:get(cberl_hostport, "localhost:8091"),
    UserName = basho_bench_config:get(cberl_username, ""),
    Password = basho_bench_config:get(cberl_password, ""),
    cberl:start_link(bench_testing, PoolSize, HostPort, UserName, Password),
    {ok, bench_testing}.

run(get, KeyGen, _ValueGen, PoolName) ->
    Key = list_to_binary(KeyGen()),
    case cberl:get(PoolName, Key) of
        {Key, _Cas, _Value} ->
            {ok, PoolName};
        {Key, {error, key_enoent}} ->
            {ok, PoolName};
        {Key, {error, Error}} ->
            {error, Error, PoolName};
        Error ->
            {error, Error, PoolName}
    end;
run(set, KeyGen, _ValueGen, PoolName) ->
    Key = list_to_binary(KeyGen()),
    Value = rand_val(50),
    case cberl:set(PoolName, Key, 60, Value) of
        ok ->
            {ok, PoolName};
        Error ->
            {error, Error, PoolName}
    end.

rand_val(N) ->
    rand_val(N, []).

rand_val(0, Acc) ->
    Acc;
rand_val(N, Acc) ->
    rand_val(N - 1, [random:uniform(26) + 96 | Acc]).
