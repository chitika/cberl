%%% @author Ali Yakamercan <aliyakamercan@gmail.com>
%%% @copyright 2012-2013 Chitika Inc.
%%% @version 0.0.2

-module(cberl).
-include("cberl.hrl").

-export([start_link/2, start_link/3, start_link/5, start_link/6]).
-export([stop/1]).
%store operations
-export([add/4, add/5, replace/4, replace/5, set/4, set/5, store/7]).
%update operations
-export([append/4, prepend/4, touch/3, mtouch/3]).
-export([incr/3, incr/4, incr/5, decr/3, decr/4, decr/5]).
-export([arithmetic/6]).
%retrieval operations
-export([get_and_touch/3, get_and_lock/3, mget/2, get/2, unlock/3, 
         mget/3, getl/3]).
%remove
-export([remove/2]).


%% @equiv start_link(PoolName, NumCon, "localhost:8091", "", "", "")
start_link(PoolName, NumCon) ->
    start_link(PoolName, NumCon, "localhost:8091", "", "", "").

%% @equiv start_link(PoolName, NumCon, Host, "", "", "")
start_link(PoolName, NumCon, Host) ->
    start_link(PoolName, NumCon, Host, "", "", "").

%% @equiv start_link(PoolName, NumCon, Host, Username, Password, "")
start_link(PoolName, NumCon, Host, Username, Password) ->
    start_link(PoolName, NumCon, Host, Username, Password, "").

%% @doc Create an instance of libcouchbase
%% hosts A list of hosts:port separated by ';' to the
%%      administration port of the couchbase cluster. (ex:
%%      "host1;host2:9000;host3" would try to connect to
%%      host1 on port 8091, if that fails it'll connect to
%%      host2 on port 9000 etc).
%% Username the username to use
%% Password The password
%% bucket The bucket to connect to
-spec start_link(atom(), integer(), string(), string(), string(), string()) -> {ok, instance()} | {error, _}.
start_link(PoolName, NumCon, Host, Username, Password, BucketName) ->
    start_link(PoolName, NumCon, Host, Username, Password, BucketName, cberl_transcoder).


start_link(PoolName, NumCon, Host, Username, Password, BucketName, Transcoder) ->
    SizeArgs = [{size, NumCon},
                {max_overflow, 10}],
    PoolArgs = [{name, {local, PoolName}},
                {worker_module, cberl_worker}] ++ SizeArgs,
    poolboy:start(PoolArgs, [Host, Username, Password, BucketName, Transcoder]).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% STORE OPERATIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

%% @equiv add(PoolName, Key, Exp, Value, standard)
-spec add(instance(), key(), integer(), value()) -> ok | {error, _}.
add(PoolName, Key, Exp, Value) ->
    add(PoolName, Key, Exp, Value, standard).

%% @equiv store(PoolName, add, Key, Value, TranscoderOpts, Exp, 0)
-spec add(instance(), key(), integer(), value(), atom()) -> ok | {error, _}.
add(PoolName, Key, Exp, Value, TranscoderOpts) ->
    store(PoolName, add, Key, Value, TranscoderOpts, Exp, 0).

%% @equiv replace(PoolName, Key, Exp, Value, standard)
-spec replace(instance(), key(), integer(), value()) -> ok | {error, _}.
replace(PoolName, Key, Exp, Value) ->
    replace(PoolName, Key, Exp, Value, standard).

%% @equiv store(PoolName, replace, "", Key, Value, Exp)
-spec replace(instance(), key(), integer(), value(), atom()) -> ok | {error, _}.
replace(PoolName, Key, Exp, Value, TranscoderOpts) ->
    store(PoolName, replace, Key, Value, TranscoderOpts, Exp, 0).

%% @equiv set(PoolName, Key, Exp, Value, standard)
-spec set(instance(), key(), integer(), value()) -> ok | {error, _}.
set(PoolName, Key, Exp, Value) ->
    set(PoolName, Key, Exp, Value, standard).

%% @equiv store(PoolName, set, "", Key, Value, Exp)
-spec set(instance(), key(), integer(), value(), integer()) -> ok | {error, _}.
set(PoolName, Key, Exp, Value, TranscoderOpts) ->
    store(PoolName, set, Key, Value, TranscoderOpts, Exp, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% UPDATE OPERATIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

-spec append(instance(), integer(), key(), value()) -> ok | {error, _}.
append(PoolName, Cas, Key, Value) ->
    store(PoolName, append, Key, Value, str, 0, Cas).

-spec prepend(instance(), integer(), key(), value()) -> ok | {error, _}.
prepend(PoolName, Cas, Key, Value) ->
    store(PoolName, prepend, Key, Value, str, 0, Cas).

%% @doc Touch (set expiration time) on the given key
%% PoolName libcouchbase instance to use
%% Key key to touch
%% ExpTime a new expiration time for the item 
-spec mtouch(instance(), key(), integer()) -> ok | {error, _}.
touch(PoolName, Key, ExpTime) ->
    {ok, Return} = mtouch(PoolName, [Key], [ExpTime]),
    {ok, hd(Return)}.

mtouch(PoolName, Keys, ExpTime) when is_integer(ExpTime) ->
    mtouch(PoolName, Keys, [ExpTime]);
mtouch(PoolName, Keys, ExpTimes) ->
    ExpTimesE = case length(Keys) - length(ExpTimes) of
        R when R > 0 ->
            ExpTimes ++ lists:duplicate(R, lists:last(ExpTimes));
        _ -> 
            ExpTimes
    end, 
    execute(PoolName, {mtouch, Keys, ExpTimesE}).

incr(PoolName, Key, OffSet) ->
    arithmetic(PoolName, Key, OffSet, 0, 0, 0).

incr(PoolName, Key, OffSet, Default) ->
    arithmetic(PoolName, Key, OffSet, 0, 1, Default).

incr(PoolName, Key, OffSet, Default, Exp) ->
    arithmetic(PoolName, Key, OffSet, Exp, 1, Default).

decr(PoolName, Key, OffSet) ->
    arithmetic(PoolName, Key, -OffSet, 0, 0, 0).

decr(PoolName, Key, OffSet, Default) ->
    arithmetic(PoolName, Key, -OffSet, 0, 1, Default).

decr(PoolName, Key, OffSet, Default, Exp) ->
    arithmetic(PoolName, Key, -OffSet, Exp, 1, Default).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RETRIEVAL METHODS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_and_touch(instance(), key(), integer()) -> {ok, integer(), value()} | {error, _}.
get_and_touch(PoolName, Key, Exp) -> 
    mget(PoolName, [Key], Exp).

-spec get(instance(), key()) -> {ok, integer(), value()} | {error, _}.
get(PoolName, Key) ->
    hd(mget(PoolName, [Key], 0)).

mget(PoolName, Keys) ->
    mget(PoolName, Keys, 0).

-spec get_and_lock(instance(), key(), integer()) -> {ok, integer(), value()} | {error, _}.
get_and_lock(PoolName, Key, Exp) ->
    getl(PoolName, Key, Exp).

-spec unlock(instance(), key(), integer()) -> ok | {error, _}.
unlock(PoolName, Key, Cas) ->
    execute(PoolName, {unlock, Key, Cas}).

%% @doc main store function takes care of all storing
%% Instance libcouchbase instance to use
%% Op add | replace | set | append | prepend
%%          add : Add the item to the cache, but fail if the object exists already
%%          replace: Replace the existing object in the cache
%%          set : Unconditionally set the object in the cache
%%          append/prepend : Append/Prepend this object to the existing object 
%% Key the key to set         
%% Value the value to set
%% Transcoder to encode the value
%% Exp When the object should expire. The expiration time is
%%     either an offset into the future.. OR an absolute
%%     timestamp, depending on how large (numerically) the
%%     expiration is. if the expiration exceeds 30 days
%%     (i.e. 24 * 3600 * 30) then it's an absolute timestamp. 
%%     pass 0 for infinity
%% CAS
-spec store(instance(), operation_type(), key(), value(), atom(), 
            integer(), integer()) -> ok | {error, _}.
store(PoolName, Op, Key, Value, TranscoderOpts, Exp, Cas) ->
    execute(PoolName, {store, Op, Key, Value,
                       TranscoderOpts, Exp, Cas}).
    
%% @doc get the value for the given key
%% Instance libcouchbase instance to use
%% HashKey the key to use for hashing 
%% Key the key to get   
%% Exp When the object should expire
%%      pass a negative number for infinity
-spec mget(instance(), key(), integer()) -> {ok, integer(), value()} | {error, _}.
mget(PoolName, Keys, Exp) ->
    execute(PoolName, {mget, Keys, Exp}).
    
%% @doc Get an item with a lock that has a timeout
%% Instance libcouchbase instance to use
%%  HashKey the key to use for hashing
%%  Key the key to get
%%  Exp When the lock should expire
-spec getl(instance(), key(), integer()) -> {ok, integer(), value()} | {error, _}.
getl(PoolName, Key, Exp) ->
    execute(PoolName, {getl, Key, Exp}).
   
%% @doc perform an arithmetic operation on the given key
%% Instance libcouchbase instance to use
%% Key key to perform on
%% Delta The amount to add / subtract
%% Exp When the object should expire
%% Create set to true if you want the object to be created if it
%%        doesn't exist.
%% Initial The initial value of the object if we create it       
-spec arithmetic(instance(), key(), integer(), integer(), integer(), integer()) ->
   ok | {error, _}.
arithmetic(PoolName, Key, OffSet, Exp, Create, Initial) ->
    execute(PoolName, {arithmetic, Key, OffSet, Exp, Create, Initial}).

%% @doc remove the value for given key
%% Instance libcouchbase instance to use
%% Key key to  remove
-spec remove(instance(), key()) -> ok | {error, _}.
remove(PoolName, Key) ->
    execute(PoolName, {remove, Key, 0}).

stop(PoolName) ->
    poolboy:stop(PoolName).

execute(PoolName, Cmd) ->
    poolboy:transaction(PoolName, fun(Worker) ->
            gen_server:call(Worker, Cmd)
       end).
