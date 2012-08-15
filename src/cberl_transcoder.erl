-module(cberl_transcoder).

-include("cberl.hrl").
-export([encode_value/2, decode_value/2, flag/1]).

-define('CBE_JSON',     16#0001).
-define('CBE_RAW',      16#0004).
-define('CBE_STR',      16#0008).

-define(STANDARD_FLAG, json).

-type encoder() :: json | raw | str.
-type encoder_list() :: [encoder()].

-spec encode_value(encoder() | encoder_list(), value()) -> value().
encode_value(Encoders, Value) ->
    encode_value1(flag(Encoders), Value).

-spec encode_value1(integer(), value()) -> value().
encode_value1(Flag, Value) when Flag band ?'CBE_STR' =/= 0 ->
    encode_value1(Flag bxor ?'CBE_STR', list_to_binary(Value));
encode_value1(Flag, Value) when Flag band ?'CBE_JSON' =/= 0 ->
    encode_value1(Flag bxor ?'CBE_JSON', jiffy:encode(Value));
encode_value1(Flag, Value) when Flag band ?'CBE_RAW' =/= 0 ->
    encode_value1(Flag bxor ?'CBE_RAW', term_to_binary(Value));
encode_value1(0, Value) ->
    Value.

-spec decode_value(integer(), value()) -> value().
decode_value(Flag, Value) when ?'CBE_RAW' band Flag =/= 0-> 
    decode_value(Flag bxor ?'CBE_RAW', binary_to_term(Value));
decode_value(Flag, Value) when ?'CBE_JSON' band Flag =/= 0-> 
    decode_value(Flag bxor ?'CBE_JSON', jiffy:decode(Value));                
decode_value(Flag, Value) when ?'CBE_STR' band Flag =/= 0-> 
    decode_value(Flag bxor ?'CBE_STR', binary_to_list(Value));
decode_value(0, Value) -> 
    Value.

-spec flag(encoder() | encoder_list()) -> integer().
flag(standard) -> flag(?STANDARD_FLAG);
flag(json) -> ?'CBE_JSON';
flag(raw_binary) -> ?'CBE_RAW';
flag(str) -> ?'CBE_STR';
flag(Encoders) when is_list(Encoders) ->
    lists:foldr(fun(Encoder, Acc) ->
                Acc bor flag(Encoder)
        end, 0, Encoders).
