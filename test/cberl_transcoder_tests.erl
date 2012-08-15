-module(cberl_transcoder_tests).
-include_lib("eunit/include/eunit.hrl").

cberl_transcoder_test_() ->
    [
        ?_assertEqual("abc", cberl_transcoder:decode_value(
            cberl_transcoder:flag(json), cberl_transcoder:encode_value(json, "abc"))),
        ?_assertEqual("abc", cberl_transcoder:decode_value(
            cberl_transcoder:flag(raw_binary), cberl_transcoder:encode_value(raw_binary, "abc"))),
        ?_assertEqual("abc", cberl_transcoder:decode_value(
            cberl_transcoder:flag(str), cberl_transcoder:encode_value(str, "abc"))),
        ?_assertEqual("abc", cberl_transcoder:decode_value(
            cberl_transcoder:flag([json, str]), cberl_transcoder:encode_value([json,str], "abc"))),
        ?_assertEqual("abc", cberl_transcoder:decode_value(
            cberl_transcoder:flag([raw_binary, str]), cberl_transcoder:encode_value([raw_binary,str], "abc")))
    ].
