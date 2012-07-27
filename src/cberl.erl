%%% @author Ali Yakamercan <aliyakamercan@gmail.com>
%%% @copyright 2012 Chitika Inc.
%%% @version 0.0.1



-module(cberl).
-include("cberl.hrl").
-export([new/0, new/1, new/3, new/4, destroy/1]).
-export([store/4, store/5, store/6, add/3, replace/3, set/3, append/3, 
         prepend/3, add/4, replace/4, set/4, append/4, prepend/4]).
-export([mget/2, mget/3, mget/4,  getl/2, getl/3, getl/4]).
-export([unlock/2, unlock/3, mtouch/3, mtouch/4,
         arithmetic/3, arithmetic/4,  arithmetic/5, arithmetic/7,
         remove/2, remove/3]).


%% @equiv new("localhost:8091", "", "", "")
new() ->
    new("localhost:8091", "", "", "").

%% @equiv new(Host, "", "", "")
new(Host) ->
    new(Host, "", "", "").

%% @equiv new(Host, Username, Password, "")
new(Host, Username, Password) ->
    new(Host, Username, Password, "").

%% @doc Create an instance of libcouchbase
%% hosts A list of hosts:port separated by ';' to the
%%      administration port of the couchbase cluster. (ex:
%%      "host1;host2:9000;host3" would try to connect to
%%      host1 on port 8091, if that fails it'll connect to
%%      host2 on port 9000 etc).
%% Username the username to use
%% Password The password
%% bucket The bucket to connect to
-spec new(string(), string(), string(), string()) -> {ok, instance()} | {error, _}.
new(Host, Username, Password, BucketName) ->
    cberl_nif:new(Host, Username, Password, BucketName).

%% @equiv add(Instance, Key, Value, 0)
-spec add(instance(), key(), value()) -> ok | {error, _}.
add(Instance, Key, Value) ->
    add(Instance, Key, Value, 0).

%% @equiv store(Instance, add, "", Key, Value, Exp)
-spec add(instance(), key(), value(), integer()) -> ok | {error, _}.
add(Instance, Key, Value, Exp) ->
    store(Instance, add, "", Key, Value, Exp).

%% @equiv replace(Instance, Key, Value, 0)
-spec replace(instance(), key(), value()) -> ok | {error, _}.
replace(Instance, Key, Value) ->
    replace(Instance, Key, Value, 0).

%% @equiv store(Instance, replace, "", Key, Value, Exp)
-spec replace(instance(), key(), value(), integer()) -> ok | {error, _}.
replace(Instance, Key, Value, Exp) ->
    store(Instance, replace, "", Key, Value, Exp).

%% @equiv set(Instance, Key, Value, 0)
-spec set(instance(), key(), value()) -> ok | {error, _}.
set(Instance, Key, Value) ->
    set(Instance, Key, Value, 0).

%% @equiv store(Instance, set, "", Key, Value, Exp)
-spec set(instance(), key(), value(), integer()) -> ok | {error, _}.
set(Instance, Key, Value, Exp) ->
    store(Instance, set, "", Key, Value, Exp).

%% @equiv append(Instance, Key, Value, 0)
-spec append(instance(), key(), value()) -> ok | {error, _}.
append(Instance, Key, Value) ->
    append(Instance, Key, Value, 0).

%% @equiv store(Instance, append, "", Key, Value, Exp)
-spec append(instance(), key(), value(), integer()) -> ok | {error, _}.
append(Instance, Key, Value, Exp) ->
    store(Instance, append, "", Key, Value, Exp).

%% @equiv prepend(Instance, Key, Value, 0)
-spec prepend(instance(), key(), value()) -> ok | {error, _}.
prepend(Instance, Key, Value) ->
    prepend(Instance, Key, Value, 0).

%% @equiv store(Instance, prepend, "", Key, Value, Exp)
-spec prepend(instance(), key(), value(), integer()) -> ok | {error, _}.
prepend(Instance, Key, Value, Exp) ->
    store(Instance, prepend, "", Key, Value, Exp).

%% @equiv store(Instance, Op, "", Key, Value, 0)
-spec store(instance(), operation_type(), key(), value()) -> ok | {error, _}.
store(Instance, Op, Key, Value) ->
    store(Instance, Op, "", Key, Value, 0).

%% @equiv store(Instance, Op, "", Key, Value, Exp)
-spec store(instance(), operation_type(), key(), value(), integer()) -> ok |
    {error, _ }.
store(Instance, Op, Key, Value, Exp) ->
    store(Instance, Op, "", Key, Value, Exp).

%% @doc main store function takes care of all storing
%% Instance libcouchbase instance to use
%% Op add | replace | set | append | prepend
%%          add : Add the item to the cache, but fail if the object exists already
%%          replace: Replace the existing object in the cache
%%          set : Unconditionally set the object in the cache
%%          append/prepend : Append/Prepend this object to the existing object 
%% HashKey the key to use for hashing
%% Key the key to set         
%% Value the value to set
%% Exp When the object should expire. The expiration time is
%%     either an offset into the future.. OR an absolute
%%     timestamp, depending on how large (numerically) the
%%     expiration is. if the expiration exceeds 30 days
%%     (i.e. 24 * 3600 * 30) then it's an absolute timestamp. 
%%     pass 0 for infinity
%% @todo Add CAS
-spec store(instance(), operation_type(), key(), key(), value(), 
            integer()) -> ok | {error, _}.
store(Instance, Op, HashKey, Key, Value, Exp) when is_list(Key) andalso 
        is_integer(Value) ->    
    cberl_nif:store(Instance, operation_value(Op), HashKey, Key, Value, Exp, ?'CBE_INT');
store(Instance, Op, HashKey, Key, Value, Exp) when is_list(Key) andalso 
        is_list(Value) ->    
    cberl_nif:store(Instance, operation_value(Op), HashKey, Key, Value, Exp, ?'CBE_STR');
store(Instance, Op, HashKey, Key, Value, Exp) when is_list(Key) andalso
        is_binary(Value) ->    
    cberl_nif:store(Instance, operation_value(Op), HashKey, Key, Value, Exp, ?'CBE_BIN');
store(_, _, _, _, _, _) ->    
    throw(badarg).

%% @equiv mget(Instance, "", Key, -1)
-spec mget(instance(), key()) -> {ok, value()} | {error, _}.
mget(Instance, Key) ->
    mget(Instance, "", Key, -1).

%% @equiv  mget(Instance, "", Key, Exp)
-spec mget(instance(), key(), integer()) -> {ok, value()} | {error, _}.
mget(Instance, Key, Exp) ->
    mget(Instance, "", Key, Exp).

%% @doc get the value for the given key
%% Instance libcouchbase instance to use
%% HashKey the key to use for hashing 
%% Key the key to get   
%% Exp When the object should expire
%%      pass a negative number for infinity
-spec mget(instance(), key(), key(), integer()) -> {ok, value()} | {error, _}.
mget(Instance, HashKey, Key, Exp) ->
    cberl_nif:mget(Instance, HashKey, Key, Exp).

%% @equiv  getl(Instance, "", Key, -1)
-spec getl(instance(), key()) -> {ok, value()} | {error, _}.
getl(Instance, Key) ->
    getl(Instance, "", Key, -1).

%% @equiv  getl(Instance, "", Key, Exp)
-spec getl(instance(), key(), integer()) -> {ok, value()} | {error, _}.
getl(Instance, Key, Exp) ->
    getl(Instance, "", Key, Exp).

%% @doc Get an item with a lock that has a timeout
%% Instance libcouchbase instance to use
%%  HashKey the key to use for hashing
%%  Key the key to get
%%  Exp When the lock should expire
-spec getl(instance(), key(), key(), integer()) -> {ok, value()} | {error, _}.
getl(Instance, HashKey, Key, Exp) ->
    cberl_nif:getl(Instance, HashKey, Key, Exp).

%% @equiv unlock(Instance, "", Key)
-spec unlock(instance(), key()) -> ok | {error, _}.
unlock(Instance, Key) ->
    unlock(Instance, "", Key).

%% @doc Unlock the key locked with GETL
%% Instance libcouchbase instance to use
%%  HashKey the key to use for hashing
%%  Key key to unlock
-spec unlock(instance(), key(), key()) -> ok | {error, _}.
unlock(Instance, HashKey, Key) ->
    cberl_nif:unlock(Instance, HashKey, Key).

%% @equiv mtouch(Instance, "", Key, ExpTime)
-spec mtouch(instance(), key(), integer()) -> ok | {error, _}.
mtouch(Instance, Key, ExpTime) ->
    mtouch(Instance, "", Key, ExpTime).

%% @doc Touch (set expiration time) on the given key
%% Instance libcouchbase instance to use
%% HashKey the key to use for hashing
%% Key key to touch
%% ExpTime a new expiration time for the item 
-spec mtouch(instance(), key(), key(), integer()) -> ok | {error, _}.
mtouch(Instance, HashKey, Key, ExpTime) ->
    cberl_nif:mtouch(Instance, HashKey, Key, ExpTime).

%% @equiv arithmetic(Instance, "", Key, Delta, 0, 0, 0)
-spec arithmetic(instance(), key(), integer()) -> ok | {error, _}.
arithmetic(Instance, Key, Delta) ->
    arithmetic(Instance, "", Key, Delta, 0, 0, 0).

%% @equiv arithmetic(Instance, "", Key, Delta, Exp, 0, 0)
-spec arithmetic(instance(), key(), integer(), integer()) -> ok | {error, _}.
arithmetic(Instance, Key, Delta, Exp) ->
    arithmetic(Instance, "", Key, Delta, Exp, 0, 0).

%% @equiv arithmetic(Instance, "", Key, Delta, Exp, 1, Initial)
-spec arithmetic(instance(), key(), integer(), integer(), integer()) -> ok | {error, _}.
arithmetic(Instance, Key, Delta, Exp, Initial) ->
    arithmetic(Instance, "", Key, Delta, Exp, 1, Initial).

%% @doc perform an arithmetic operation on the given key
%% Instance libcouchbase instance to use
%% HashKey the key to use for hashing
%% Key key to perform on
%% Delta The amount to add / subtract
%% Exp When the object should expire
%% Create set to true if you want the object to be created if it
%%        doesn't exist.
%% Initial The initial value of the object if we create it       
-spec arithmetic(instance(), key(), key(), integer(), integer(), integer(), integer()) ->
   ok | {error, _}.
arithmetic(Instance, HashKey, Key, Delta, Exp, Create, Initial) ->
    cberl_nif:arithmetic(Instance, HashKey, Key, Delta, Exp, Create, Initial).

%% @equiv remove(Instance, "", Key)
-spec remove(instance(), key()) -> ok | {error, _}.
remove(Instance, Key) ->
    remove(Instance, "", Key).

%% @doc remove the value for given key
%% Instance libcouchbase instance to use
%% HashKey the key to use for hashing
%% Key key to  remove
-spec remove(instance(), key(), key()) -> ok | {error, _}.
remove(Instance, HashKey, Key) ->
    cberl_nif:remove(Instance, HashKey, Key).

%% @doc close the libcouchbase instance
destroy(Instance) ->
    cberl_nif:destroy(Instance).

-spec operation_value(operation_type()) -> integer().
operation_value(add) -> ?'CBE_ADD';
operation_value(replace) -> ?'CBE_REPLACE';
operation_value(set) -> ?'CBE_SET';
operation_value(append) -> ?'CBE_APPEND';
operation_value(prepend) -> ?'CBE_PREPEND'.
