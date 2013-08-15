-module(cberl_worker).
-behaviour(poolboy_worker).
-include("cberl.hrl").
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(TIMEOUT, infinity).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Args) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Host, Username, Password, BucketName, Transcoder]) ->
    process_flag(trap_exit, true),
    {ok, Handle} = cberl_nif:new(),
    ok = cberl_nif:control(Handle, op(connect), [Host, Username, Password, BucketName]),
    receive
        ok -> {ok, #instance{handle = Handle, transcoder = Transcoder}};
        {error, Error} -> {stop, Error}
    after ?TIMEOUT -> {error, timeout}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({mtouch, Keys, ExpTimesE}, _From, 
            State = #instance{handle = Handle}) ->
    ok = cberl_nif:control(Handle, op(mtouch), [Keys, ExpTimesE]),
    receive
        Reply -> {reply, Reply, State}
    after ?TIMEOUT -> {reply, {error, timeout}, State}
    end;
handle_call({unlock, Key, Cas}, _From, 
            State = #instance{handle = Handle}) ->
    {reply, cberl_nif:unlock(Handle, Key, Cas), State};
handle_call({store, Op, Key, Value, TranscoderOpts, Exp, Cas}, _From, 
            State = #instance{handle = Handle, transcoder = Transcoder}) ->
    StoreValue = Transcoder:encode_value(TranscoderOpts, Value), 
    ok = cberl_nif:control(Handle, op(store), [operation_value(Op), Key, StoreValue, 
                    Transcoder:flag(TranscoderOpts), Exp, Cas]),
    receive
        Reply -> {reply, Reply, State}
    after ?TIMEOUT -> {reply, {error, timeout}, State}
    end;
handle_call({mget, Keys, Exp}, _From, 
            State = #instance{handle = Handle, transcoder = Transcoder}) ->
    ok = cberl_nif:control(Handle, op(mget), [Keys, Exp]),
    Reply = receive
        {error, Error} -> {error, Error};
        {ok, Results} ->
            lists:map(fun(Result) ->
                        case Result of
                            {Cas, Flag, Key, Value} ->
                                DecodedValue = Transcoder:decode_value(Flag, Value),
                                {Key, Cas, DecodedValue};
                            {_Key, {error, _Error}} ->
                                Result
                        end
                end, Results)
    after ?TIMEOUT -> {error, timeout}
    end,
    {reply, Reply, State};
handle_call({getl, Key, Exp}, _From,
            State = #instance{handle = Handle, transcoder = Transcoder}) ->
    ok = cberl_nif:control(Handle, op(getl), [Key, Exp]),
    Reply = receive
        {error, Error} -> {error, Error};
        {ok, {Cas, Flag, Value}} ->
            DecodedValue = Transcoder:decode_value(Flag, Value),
            {ok, Cas, DecodedValue}
    after ?TIMEOUT -> {error, timeout}
    end,
    {reply, Reply, State};
handle_call({arithmetic, Key, OffSet, Exp, Create, Initial}, _From,
            State = #instance{handle = Handle, transcoder = Transcoder}) ->
    ok = cberl_nif:control(Handle, op(arithmetic), [Key, OffSet, Exp, Create, Initial]),
    Reply = receive
        {error, Error} -> {error, Error};
        {ok, {Cas, Flag, Value}} ->
            DecodedValue = Transcoder:decode_value(Flag, Value),
            {ok, Cas, DecodedValue}
    after ?TIMEOUT -> {error, timeout}
    end,
    {reply, Reply, State};
handle_call({remove, Key, N}, _From,
            State = #instance{handle = Handle}) ->
    ok = cberl_nif:control(Handle, op(remove), [Key, N]),
    receive
        Reply -> {reply, Reply, State}
    after ?TIMEOUT -> {error, timeout}
    end;
handle_call({http, Path, Body, ContentType, Method, Chunked}, _From,
            State = #instance{handle = Handle}) ->
    ok = cberl_nif:control(Handle, op(http), [Path, Body, ContentType, Method, Chunked]),
    receive
        Reply -> {reply, Reply, State}
    after ?TIMEOUT -> {error, timeout}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State = #instance{handle = Handle}) ->
    cberl_nif:destroy(Handle),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec operation_value(operation_type()) -> integer().
operation_value(add) -> ?'CBE_ADD';
operation_value(replace) -> ?'CBE_REPLACE';
operation_value(set) -> ?'CBE_SET';
operation_value(append) -> ?'CBE_APPEND';
operation_value(prepend) -> ?'CBE_PREPEND'.

op(connect) -> 0;
op(store) -> 1;
op(mget) -> 2;
op(getl) -> 3;
op(unlock) -> 4;
op(mtouch) -> 5;
op(arithmetic) -> 6;
op(remove) -> 7;
op(http) -> 8.
