%%% @copyright (C) 2018, Relayr
-module(jaeger_passage_thrift_tests).
-author("pbober").

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

make_emit_batch_message_test() ->
    TraceId = 256,
    SpanId = 8,
    meck:new(rand, [passthrough, non_strict, unstick]),
    meck:expect(rand, uniform,
        fun
            (16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF) -> TraceId;
            (16#FFFFFFFFFFFFFFFF) -> SpanId;
            (Arg) -> meck:passthrough([Arg])
        end
    ),

    meck:new(passage_span, [passthrough, non_strict]),
    meck:expect(passage_span, get_finish_time, fun(_Span) -> {ok, {1, 2, 3}} end),

    ContextState = jaeger_passage_span_context:make_span_context_state([]),
    Context = passage_span_context:make(ContextState, #{"baggage-key" => "baggage value"}),
    Span = passage_span:make_extracted_span(test_tracer, Context),

    ServiceName = test_service,
    Tags = #{'erlang.node' => node(), aaa => "test"},
    Message = jaeger_passage_thrift:make_emit_batch_message(ServiceName, Tags, [Span]),

    EncodedBinary = thrift_protocol:encode_message(Message, binary),
    EncodedCompact = thrift_protocol:encode_message(Message, compact),

    ?assertEqual(expected_binary(), EncodedBinary),
    ?assertEqual(expected_compact(), EncodedCompact),

    DecodedBinary = thrift_protocol:decode_message(iolist_to_binary(EncodedBinary), binary),
    DecodedCompact = thrift_protocol:decode_message(iolist_to_binary(EncodedCompact), compact),

    ?assertEqual({Message, <<>>}, DecodedBinary),
    ?assertEqual({Message, <<>>}, DecodedCompact).

%%------------------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------------------

expected_binary() ->
    [<<128, 1, 0, 4, 0, 0, 0, 9, 101, 109, 105, 116, 66, 97, 116, 99, 104, 0, 0, 0, 0>>,
        <<12, 0, 1>>,
        [<<15, 0, 2>>,
            [<<12, 0, 0, 0, 1>>,
                [<<15, 0, 11>>,
                    [<<12, 0, 0, 0, 0>>],
                    <<15, 0, 10>>,
                    [<<12, 0, 0, 0, 0>>],
                    <<10, 0, 9>>,
                    <<0, 0, 0, 232, 212, 195, 148, 131>>,
                    <<10, 0, 8>>,
                    <<0, 0, 0, 0, 0, 0, 0, 0>>,
                    <<8, 0, 7>>,
                    <<0, 0, 0, 1>>,
                    <<15, 0, 6>>,
                    [<<12, 0, 0, 0, 0>>],
                    <<11, 0, 5>>,
                    [<<0, 0, 0, 9>> | <<"undefined">>],
                    <<10, 0, 4>>,
                    <<0, 0, 0, 0, 0, 0, 0, 0>>,
                    <<10, 0, 3>>,
                    <<0, 0, 0, 0, 0, 0, 0, 8>>,
                    <<10, 0, 2>>,
                    <<0, 0, 0, 0, 0, 0, 0, 0>>,
                    <<10, 0, 1>>,
                    <<0, 0, 0, 0, 0, 0, 1, 0>>,
                    <<0>>]],
            <<12, 0, 1>>,
            [<<15, 0, 2>>,
                [<<12, 0, 0, 0, 2>>,
                    [<<11, 0, 3>>,
                        [<<0, 0, 0, 13>> | <<"nonode@nohost">>],
                        <<8, 0, 2>>,
                        <<0, 0, 0, 0>>,
                        <<11, 0, 1>>,
                        [<<0, 0, 0, 11>> | <<"erlang.node">>],
                        <<0>>],
                    [<<11, 0, 3>>,
                        [<<0, 0, 0, 4>> | <<"test">>],
                        <<8, 0, 2>>,
                        <<0, 0, 0, 0>>,
                        <<11, 0, 1>>,
                        [<<0, 0, 0, 3>> | <<"aaa">>],
                        <<0>>]],
                <<11, 0, 1>>,
                [<<0, 0, 0, 12>> | <<"test_service">>],
                <<0>>],
            <<0>>],
        <<0>>].

expected_compact() ->
    [<<130, 129>>,
        <<0>>,
        [<<"\t">> | <<"emitBatch">>],
        <<28>>,
        [<<28>>,
            [<<24>>,
                [<<"\f">> | <<"test_service">>],
                <<25>>,
                [<<",">>,
                    [<<24>>,
                        [<<"\v">> | <<"erlang.node">>],
                        <<21>>,
                        <<0>>,
                        <<24>>,
                        [<<"\r">> | <<"nonode@nohost">>] |
                        <<0>>],
                    [<<24>>,
                        [<<3>> | <<"aaa">>],
                        <<21>>,
                        <<0>>,
                        <<24>>,
                        [<<4>> | <<"test">>] |
                        <<0>>]] |
                <<0>>],
            <<25>>,
            [<<28>>,
                [<<22>>,
                    <<128, 4>>,
                    <<22>>,
                    <<0>>,
                    <<22>>,
                    <<16>>,
                    <<22>>,
                    <<0>>,
                    <<24>>,
                    [<<"\t">> | <<"undefined">>],
                    <<25>>,
                    [<<"\f">>],
                    <<21>>,
                    <<2>>,
                    <<22>>,
                    <<0>>,
                    <<22>>,
                    <<134, 210, 156, 204, 154, 58>>,
                    <<25>>,
                    [<<"\f">>],
                    <<25>>,
                    [<<"\f">>] |
                    <<0>>]] |
            <<0>>] |
        <<0>>].
