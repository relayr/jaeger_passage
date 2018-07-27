%%% @copyright (C) 2018, Relayr
-module(jaeger_passage_thrift_tests).
-author("pbober").

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

send_thrift_test() ->
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

    meck:new(gen_udp, [passthrough, non_strict, unstick]),
    meck:expect(gen_udp, open, fun(_Port, _Opts) -> {ok, udp_socket} end),
    meck:expect(gen_udp, send, fun(_Socket, _Host, _Port, _Data) -> ok end),
    meck:expect(gen_udp, close, fun(_Socket) -> ok end),

    ContextState = jaeger_passage_span_context:make_span_context_state([]),
    Context = passage_span_context:make(ContextState, #{"baggage-key" => "baggage value"}),
    Span = passage_span:make_extracted_span(test_tracer, Context),

    ServiceName = test_service,
    Tags = #{'erlang.node' => node(), zzz => "test"},

    %% binary format
    {ok, ClientBin} = jaeger_passage_thrift:start_client("localhost", 10001, binary),
    ClientBin1 = jaeger_passage_thrift:send_report(ServiceName, Tags, Span, ClientBin),
    ?assertEqual(expected_binary(), iolist_to_binary(extract_sent_data())),
    ok = jaeger_passage_thrift:stop_client(ClientBin1),

%%    %% compact format
%%    {ok, ClientComp} = jaeger_passage_thrift:start_client("localhost", 10002, compact),
%%    ClientComp1 = jaeger_passage_thrift:send_report(ServiceName, Tags, Span, ClientComp),
%%    ?assertEqual(expected_compact(), iolist_to_binary(extract_sent_data())),
%%    ok = jaeger_passage_thrift:stop_client(ClientComp1),

    ok.

%%------------------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------------------

extract_sent_data() ->
    Args = [Args || {_Pid, {Mod, Func, Args}, _Result} <- meck:history(gen_udp), Mod =:= gen_udp, Func =:= send],
    [[_Socket, _Host, _Port, SentData]] = Args,
    SentData.

expected_binary() ->
    %% generated with thrift_protocol_binary from https://github.com/sile/thrift_protocol
    %% with a patch that eliminated field reversing in struct and map
    <<128,1,0,4,0,0,0,9,101,109,105,116,66,97,116,99,104,0,0,0,0,12,0,1,12,0,1,11,
        0,1,0,0,0,12,116,101,115,116,95,115,101,114,118,105,99,101,15,0,2,12,0,0,0,2,
        11,0,1,0,0,0,11,101,114,108,97,110,103,46,110,111,100,101,8,0,2,0,0,0,0,11,0,
        3,0,0,0,13,110,111,110,111,100,101,64,110,111,104,111,115,116,0,11,0,1,0,0,0,
        3,122,122,122,8,0,2,0,0,0,0,11,0,3,0,0,0,4,116,101,115,116,0,0,15,0,2,12,0,0,0,
        1,10,0,1,0,0,0,0,0,0,1,0,10,0,2,0,0,0,0,0,0,0,0,10,0,3,0,0,0,0,0,0,0,8,10,0,
        4,0,0,0,0,0,0,0,0,11,0,5,0,0,0,9,117,110,100,101,102,105,110,101,100,15,0,6,
        12,0,0,0,0,8,0,7,0,0,0,1,10,0,8,0,0,0,0,0,0,0,0,10,0,9,0,0,0,232,212,195,148,
        131,15,0,10,12,0,0,0,0,15,0,11,12,0,0,0,0,0,0,0>>.

%%expected_compact() ->
%%    <<130,129,0,9,101,109,105,116,66,97,116,99,104,28,28,24,12,116,101,115,116,95,
%%        115,101,114,118,105,99,101,25,44,24,11,101,114,108,97,110,103,46,110,111,100,
%%        101,21,0,24,13,110,111,110,111,100,101,64,110,111,104,111,115,116,0,24,3,97,
%%        97,97,21,0,24,4,116,101,115,116,0,0,25,28,22,128,4,22,0,22,16,22,0,24,9,117,
%%        110,100,101,102,105,110,101,100,25,12,21,2,22,0,22,134,210,156,204,154,58,25,
%%        12,25,12,0,0,0>>.
