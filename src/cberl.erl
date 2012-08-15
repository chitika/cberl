%%% @author Ali Yakamercan <aliyakamercan@gmail.com>
%%% @copyright 2012 Chitika Inc.
%%% @version 0.0.1



-module(cberl).
-include("cberl.hrl").
-export([new/0, new/1, new/3, new/4, remove/2, destroy/1]).
-export([store/7, mget/3, getl/3, arithmetic/6]).
%store operations
-export([add/4, add/5, replace/4, replace/5, set/4, set/5]).
%update operations
-export([append/4, prepend/4, mtouch/3]).
-export([incr/3, incr/4, incr/5, decr/3, decr/4, decr/5]).
%retrieval operations
-export([get_and_touch/3, get_and_lock/3, get/2, unlock/3]).


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


%%%%%%%%%%%%%%%%%%%%%%%%
%%% STORE OPERATIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

%% @equiv add(Instance, Key, Exp, Value, json)
-spec add(instance(), key(), integer(), value()) -> ok | {error, _}.
add(Instance, Key, Exp, Value) ->
    add(Instance, Key, Exp, Value, json).

%% @equiv store(Instance, add, Key, Value, Transcoder, Exp, 0)
-spec add(instance(), key(), integer(), value(), atom()) -> ok | {error, _}.
add(Instance, Key, Exp, Value, Transcoder) ->
    store(Instance, add, Key, Value, Transcoder, Exp, 0).

%% @equiv replace(Instance, Key, Exp, Value, json)
-spec replace(instance(), key(), integer(), value()) -> ok | {error, _}.
replace(Instance, Key, Exp, Value) ->
    replace(Instance, Key, Exp, Value, json).

%% @equiv store(Instance, replace, "", Key, Value, Exp)
-spec replace(instance(), key(), integer(), value(), atom()) -> ok | {error, _}.
replace(Instance, Key, Exp, Value, Transcoder) ->
    store(Instance, replace, Key, Value, Transcoder, Exp, 0).

%% @equiv set(Instance, Key, Exp, Value, json)
-spec set(instance(), key(), integer(), value()) -> ok | {error, _}.
set(Instance, Key, Exp, Value) ->
    set(Instance, Key, Exp, Value, json).

%% @equiv store(Instance, set, "", Key, Value, Exp)
-spec set(instance(), key(), integer(), value(), integer()) -> ok | {error, _}.
set(Instance, Key, Exp, Value, Transcoder) ->
    store(Instance, set, Key, Value, Transcoder, Exp, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% UPDATE OPERATIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

%% @equiv append(Instance, Key, Value, 0)
-spec append(instance(), integer(), key(), value()) -> ok | {error, _}.
append(Instance, Cas, Key, Value) ->
    store(Instance, append, Key, Value, str, 0, Cas).

%% @equiv prepend(Instance, Cas, Key, Value, json)
-spec prepend(instance(), integer(), key(), value()) -> ok | {error, _}.
prepend(Instance, Cas, Key, Value) ->
    store(Instance, prepend, Key, Value, str, 0, Cas).

%% @doc Touch (set expiration time) on the given key
%% Instance libcouchbase instance to use
%% Key key to touch
%% ExpTime a new expiration time for the item 
-spec mtouch(instance(), key(), integer()) -> ok | {error, _}.
mtouch(Instance, Key, ExpTime) ->
    cberl_nif:mtouch(Instance, Key, ExpTime).

incr(Instance, Key, OffSet) ->
    arithmetic(Instance, Key, OffSet, 0, 0, 0).

incr(Instance, Key, OffSet, Default) ->
    arithmetic(Instance, Key, OffSet, 0, 1, Default).

incr(Instance, Key, OffSet, Default, Exp) ->
    arithmetic(Instance, Key, OffSet, Exp, 1, Default).

decr(Instance, Key, OffSet) ->
    arithmetic(Instance, Key, -OffSet, 0, 0, 0).

decr(Instance, Key, OffSet, Default) ->
    arithmetic(Instance, Key, -OffSet, 0, 1, Default).

decr(Instance, Key, OffSet, Default, Exp) ->
    arithmetic(Instance, Key, -OffSet, Exp, 1, Default).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RETRIEVAL METHODS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_and_touch(instance(), key(), integer()) -> {ok, integer(), value()} | {error, _}.
get_and_touch(Instance, Key, Exp) -> 
    mget(Instance, Key, Exp).

-spec get(instance(), key()) -> {ok, integer(), value()} | {error, _}.
get(Instance, Key) ->
    mget(Instance, Key, 0).

-spec get_and_lock(instance(), key(), integer()) -> {ok, integer(), value()} | {error, _}.
get_and_lock(Instance, Key, Exp) ->
    getl(Instance, Key, Exp).

-spec unlock(instance(), key(), integer()) -> ok | {error, _}.
unlock(Instance, Key, Cas) ->
    cberl_nif:unlock(Instance, Key, Cas).
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
store(Instance, Op, Key, Value, Transcoder, Exp, Cas) ->
    StoreValue = encode_value(Transcoder, Value), 
    cberl_nif:store(Instance, operation_value(Op), Key, StoreValue, 
                    transcoder_flag(Transcoder), Exp, Cas).

%% @doc get the value for the given key
%% Instance libcouchbase instance to use
%% HashKey the key to use for hashing 
%% Key the key to get   
%% Exp When the object should expire
%%      pass a negative number for infinity
-spec mget(instance(), key(), integer()) -> {ok, integer(), value()} | {error, _}.
mget(Instance, Key, Exp) ->
    case cberl_nif:mget(Instance, Key, Exp) of
        {error, Error} -> {error, Error};
        {ok, {Cas, Flag, Value}} ->
            DecodedValue = decode_value(Flag, Value),
            {ok, Cas, DecodedValue}
    end.

%% @doc Get an item with a lock that has a timeout
%% Instance libcouchbase instance to use
%%  HashKey the key to use for hashing
%%  Key the key to get
%%  Exp When the lock should expire
-spec getl(instance(), key(), integer()) -> {ok, integer(), value()} | {error, _}.
getl(Instance, Key, Exp) ->
    case cberl_nif:getl(Instance, Key, Exp) of
        {error, Error} -> {error, Error};
        {ok, {Cas, Flag, Value}} ->
            DecodedValue = decode_value(Flag, Value),
            {ok, Cas, DecodedValue}
    end.
   
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
arithmetic(Instance, Key, OffSet, Exp, Create, Initial) ->
    case cberl_nif:arithmetic(Instance, Key, OffSet, Exp, Create, Initial) of
        {error, Error} -> {error, Error};
        {ok, {Cas, Flag, Value}} ->
            DecodedValue = decode_value(Flag, Value),
            {ok, Cas, DecodedValue}
    end.

%% @doc remove the value for given key
%% Instance libcouchbase instance to use
%% Key key to  remove
-spec remove(instance(), key()) -> ok | {error, _}.
remove(Instance, Key) ->
    cberl_nif:remove(Instance, Key, 0).

%% @doc close the libcouchbase instance
destroy(Instance) ->
    cberl_nif:destroy(Instance).


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% INTERNAL FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec operation_value(operation_type()) -> integer().
operation_value(add) -> ?'CBE_ADD';
operation_value(replace) -> ?'CBE_REPLACE';
operation_value(set) -> ?'CBE_SET';
operation_value(append) -> ?'CBE_APPEND';
operation_value(prepend) -> ?'CBE_PREPEND'.

encode_value(json, Value) -> jiffy:encode(Value);
encode_value(gzip, _Value) -> <<"zipped value here">>;
encode_value(raw_binary, Value) -> term_to_binary(Value);
encode_value(Custom, Value) ->
    {_Flag, {Module, Function}} = application:get_env(Custom),
    apply(Module, Function, [Value]).

decode_value(?'CBE_JSON', Value) -> jiffy:decode(Value);
decode_value(?'CBE_GZIP', Value) -> <<"unzipped value here">>;
decode_value(?'CBE_RAW', Value) -> binary_to_term(Value).

%decode_value(Custom, Value) -> jsx:to_term(Value);
transcoder_flag(json) -> ?'CBE_JSON';
transcoder_flag(gzip) -> ?'CBE_GZIP';
transcoder_flag(raw_binary) -> ?'CBE_RAW'.

%transcoder_flag(Custom) -> 
%    {Flag, {_Module, _Function}} = application:get_env(Custom),
%    Flag.

