%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Thrift messages for jaeger
%%
%% IDL:
%% - https://github.com/jaegertracing/jaeger-idl/blob/b59e84eb/thrift/agent.thrift
%% - https://github.com/jaegertracing/jaeger-idl/blob/b59e84eb/thrift/jaeger.thrift
%%
%% @private
-module(jaeger_passage_thrift).

-include("thrift/jaeger_types.hrl").
-include("constants.hrl").

-export([start_client/3, send_report/4, stop_client/1]).

-type format() :: binary | compact.
-type client() :: term(). % #tclient{} from thrift_client

-export_type([format/0, client/0]).

%%------------------------------------------------------------------------------
%% Application Internal API
%%------------------------------------------------------------------------------

-spec start_client(inet:hostname(), inet:port_number(), format()) -> {ok, client()} | {error, term()}.
start_client(AgentHost, AgentPort, Format) ->
    {ok, TransportFactory} = jaeger_passage_thrift_transport:new_transport_factory(AgentHost, AgentPort, #{}),
    {ok, ProtocolFactory} = case Format of
                                binary ->
                                    thrift_binary_protocol:new_protocol_factory(TransportFactory, []);
                                compact ->
                                    % TODO implement compact format support (requires thrift library update to 0.10+)
%%                                    thrift_compact_protocol:new_protocol_factory(TransportFactory, [])
                                    {error, {unsupported_thrift_format, Format}}
                            end,
    case ProtocolFactory() of
        {ok, Protocol} ->
            thrift_client:new(Protocol, agent_thrift);
        {error, _} = Error ->
            Error
    end.

-spec send_report(atom(), passage:tags(), passage_span:span(), client()) -> client().
send_report(Name, Tags, Span, Client) ->
    Process = #'Process'{serviceName = atom_to_binary(Name, utf8), tags = make_tags(Tags)},
    Batch = #'Batch'{process = Process, spans = [make_span(Span)]},
    % TODO: update thrift library and use plain thrift_client:send_call/3
%%    {Client1, ok} = thrift_client:send_call(Client, emitBatch, [Batch]),
    {Client1, ok} = send_function_call(Client, emitBatch, [Batch]),
    Client1.

-spec stop_client(client()) -> ok.
stop_client(Client) ->
    thrift_client:close(Client),
    ok.

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------

-spec make_tags(passage:tags()) -> ['Tag'()].
make_tags(Tags) ->
    [make_tag(K, V) || {K, V} <- maps:to_list(Tags)].

-spec make_tag(passage:tag_name(), passage:tag_value()) -> 'Tag'().
make_tag(Key, Value) ->
    Tag = #'Tag'{
        key = atom_to_binary(Key, utf8),
        vType = ?JAEGER_TAGTYPE_STRING % set as default
    },
    make_tag_value(Tag, Value).

-spec make_tag_value('Tag'(), term()) -> 'Tag'().
make_tag_value(Tag, X) when is_boolean(X) -> Tag#'Tag'{vType = ?JAEGER_TAGTYPE_BOOL, vBool = X};
make_tag_value(Tag, X) when is_atom(X)    -> Tag#'Tag'{vType = ?JAEGER_TAGTYPE_STRING, vStr = atom_to_binary(X, utf8)};
make_tag_value(Tag, X) when is_binary(X)  -> Tag#'Tag'{vType = ?JAEGER_TAGTYPE_STRING, vStr = X};
make_tag_value(Tag, X) when is_float(X)   -> Tag#'Tag'{vType = ?JAEGER_TAGTYPE_DOUBLE, vDouble = X};
make_tag_value(Tag, X) when is_integer(X) -> Tag#'Tag'{vType = ?JAEGER_TAGTYPE_LONG, vLong = X};
make_tag_value(Tag, X) ->
    Size = erlang:external_size(X),
    case Size > 1024 of
        true ->
            Binary = list_to_binary(io_lib:format("...~p bytes... (ommitted by ~p)", [Size, ?MODULE])),
            Tag#'Tag'{vType = ?JAEGER_TAGTYPE_STRING, vStr = Binary};
        false ->
            try list_to_binary(X) of
                Binary ->
                    Tag#'Tag'{vType = ?JAEGER_TAGTYPE_STRING, vStr = Binary}
            catch
                error:badarg ->
                    Binary = list_to_binary(io_lib:format("~1024p", [X])),
                    Tag#'Tag'{vType = ?JAEGER_TAGTYPE_STRING, vStr = Binary}
            end
    end.

-spec make_span(passage_span:span()) -> 'Span'().
make_span(Span) ->
    Context = passage_span:get_context(Span),
    TraceId = jaeger_passage_span_context:get_trace_id(Context),
    Refs =
        lists:filter(
          fun ({_, S}) ->
                  0 =/= jaeger_passage_span_context:get_span_id(passage_span:get_context(S))
          end,
          passage_span:get_refs(Span)),
    ParentSpanId =
        case Refs of
            [{_, Parent} | _] ->
                jaeger_passage_span_context:get_span_id(
                  passage_span:get_context(Parent));
            _ -> 0
        end,
    Tags0 = passage_span:get_tags(Span),
    Tags1 =
        case jaeger_passage_span_context:get_debug_id(Context) of
            error         -> Tags0;
            {ok, DebugId} -> maps:put(?JAEGER_DEBUG_HEADER, DebugId, Tags0)
        end,
    #'Span'{
        traceIdLow = to_i64(TraceId),
        traceIdHigh = to_i64(TraceId bsr 64),
        spanId = to_i64(jaeger_passage_span_context:get_span_id(Context)),
        parentSpanId = to_i64(ParentSpanId),
        operationName = atom_to_binary(passage_span:get_operation_name(Span), utf8),
        references = make_references(Refs),
        flags = jaeger_passage_span_context:get_flags(Context),
        startTime = timestamp_to_us(passage_span:get_start_time(Span)),
        duration = get_duration_us(Span),
        tags = make_tags(Tags1),
        logs = lists:map(fun make_log/1, passage_span:get_logs(Span))
    }.

-spec make_references(passage:refs()) -> ['SpanRef'()].
make_references(Refs) ->
    lists:filtermap(
        fun(Ref = {_, Span}) ->
            Context = passage_span:get_context(Span),
            case jaeger_passage_span_context:get_span_id(Context) of
                0 -> false;
                _ -> {true, make_reference(Ref)}
            end
        end,
        Refs).

-spec make_reference(passage:ref()) -> 'SpanRef'().
make_reference({_, Span} = Ref) ->
    RefType =
        case Ref of
            {child_of, _}     -> ?JAEGER_SPANREFTYPE_CHILD_OF;
            {follows_from, _} -> ?JAEGER_SPANREFTYPE_FOLLOWS_FROM
        end,
    Context = passage_span:get_context(Span),
    TraceId = jaeger_passage_span_context:get_trace_id(Context),
    SpanId = jaeger_passage_span_context:get_span_id(Context),
    #'SpanRef'{
        refType = RefType,
        traceIdLow = to_i64(TraceId),
        traceIdHigh = to_i64(TraceId bsr 64),
        spanId = to_i64(SpanId)
    }.

-spec make_log(passage_span:log()) -> 'Log'().
make_log({Fields, Time}) ->
    #'Log'{timestamp = timestamp_to_us(Time), fields = make_tags(Fields)}.

-spec timestamp_to_us(erlang:timestamp()) -> non_neg_integer().
timestamp_to_us(Timestamp) ->
    timer:now_diff(Timestamp, {0, 0, 0}).

-spec get_duration_us(passage_span:span()) -> non_neg_integer().
get_duration_us(Span) ->
    Start = passage_span:get_start_time(Span),
    {ok, Finish} = passage_span:get_finish_time(Span),
    timer:now_diff(Finish, Start).

-spec to_i64(non_neg_integer()) -> integer().
to_i64(N) ->
    <<S:64/signed>> = <<N:64>>,
    S.


%%-------------------------------------------------------------------------------------------------------------
%% This function has been copied to override a bug in thrift library (it is already fixed in 0.10)
%%-------------------------------------------------------------------------------------------------------------

-record(tclient, {service :: module(), protocol :: term(), seqid :: non_neg_integer()}).
-record(protocol_message_begin, {name :: string(), type :: non_neg_integer(), seqid :: non_neg_integer()}).

-define(tMessageType_CALL, 1).
-define(tMessageType_ONEWAY, 4).

-spec send_function_call(#tclient{}, atom(), list()) -> {client(), ok | {error, any()}}.
send_function_call(Client, Function, Args) ->
    #tclient{service = Service, protocol = Proto0, seqid = SeqId} = Client,
    Params = try Service:function_info(Function, params_type)
             catch error:function_clause -> no_function
             end,
    case Params of
        no_function ->
            {Client, {error, {no_function, Function}}};
        {struct, PList} when length(PList) =/= length(Args) ->
            {Client, {error, {bad_args, Function, Args}}};
        {struct, _PList} ->
            % bug was here: always setting "call" type
            MsgType = case Service:function_info(Function, reply_type) of
                          oneway_void -> ?tMessageType_ONEWAY;
                          _ -> ?tMessageType_CALL
                      end,
            Begin = #protocol_message_begin{name = atom_to_list(Function), type = MsgType, seqid = SeqId},
            {Proto1, ok} = thrift_protocol:write(Proto0, Begin),
            {Proto2, ok} = thrift_protocol:write(Proto1, {Params, list_to_tuple([Function | Args])}),
            {Proto3, ok} = thrift_protocol:write(Proto2, message_end),
            {Proto4, ok} = thrift_protocol:flush_transport(Proto3),
            {Client#tclient{protocol = Proto4}, ok}
    end.
