%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A reporter that sends the spans to an jaeger agent
%%
%% === Examples ===
%%
%% ```
%% %% Starts `example_reporter'
%% {ok, Reporter} = jaeger_passage_reporter:start(example_reporter).
%% [example_reporter] = jaeger_passage_reporter:which_reporters().
%%
%% %% Registers `example_tracer'
%% Context = jaeger_passage_span_context.
%% Sampler = passage_sampler_all:new().
%% ok = passage_tracer_registry:register(example_tracer, Context, Sampler, Reporter).
%%
%% %% Starts and finishes a span
%% Span = passage:start_span(example, [{tracer, example_tracer}]).
%%
%% passage:finish_span(Span). % The span will send to the jaeger agent on the localhost
%% '''
%%
%% === Refereces ===
%%
%% <ul>
%% <li><a href="http://jaeger.readthedocs.io/en/latest/architecture/#agent">Jaeger - Architecture - Agent</a></li>
%% <li><a href="http://jaeger.readthedocs.io/en/latest/deployment/#agent">Jaeger - Deployment - Agent</a></li>
%% </ul>
-module(jaeger_passage_reporter).

-behaviour(passage_reporter).
-behaviour(gen_server).

-include("constants.hrl").

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([start/1, start/2]).
-export([stop/1]).
-export([which_reporters/0]).
-export([get_id/1]).

-export_type([reporter_id/0]).
-export_type([start_option/0, start_options/0]).

%%------------------------------------------------------------------------------
%% Application Internal API
%%------------------------------------------------------------------------------
-export([start_link/2]).

%%------------------------------------------------------------------------------
%% 'passage_reporter' Callback API
%%------------------------------------------------------------------------------
-export([report/2]).

%%------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%------------------------------------------------------------------------------
%% Macros & Records
%%------------------------------------------------------------------------------
-define(STATE, ?MODULE).

-record(?STATE,
        {
          thrift_client        :: jaeger_passage_thrift:client(),
          default_service_name :: atom(),
          process_tags         :: passage:tags()
        }).

%%------------------------------------------------------------------------------
%% Exported Types
%%------------------------------------------------------------------------------
-type reporter_id() :: atom().
%% Reporter identifier.

-type start_options() :: [start_option()].
%% Options for {@link start/2}.

-type start_option() :: {thrift_format, jaeger_passage_thrift:format()}
                      | {agent_host, inet:hostname()}
                      | {agent_port, inet:port_number()}
                      | {default_service_name, atom()}
                      | {process_tags, passage:tags()}.
%% <ul>
%%   <li><b>thrift_format</b>: The format for encoding thrift messages. The default value is `binary'.</li>
%%   <li><b>agent_host</b>: The hostname of the jaeger agent. The default value is `"127.0.0.1"'.</li>
%%   <li><b>agent_port</b>: The port of the jaeger agent. The default values for the thrift format `compact' and `binary' are `6831' and `6832' respectively.</li>
%%   <li><b>default_service_name</b>: The default service name. If a reporting span has `location.application' tag, the value is used as the service name instead of this. The default value is `ReporterId'.</li>
%%   <li><b>process_tags</b>: The tags of the reporting process. The default value is `#{}'.</li>
%% </ul>

%%------------------------------------------------------------------------------
%% Application Internal Functions
%%------------------------------------------------------------------------------
%% @private
-spec start_link(reporter_id(), start_options()) -> {ok, pid()} | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start_link(ReporterId, Options) ->
    Name = jaeger_passage_local_ns:reporter_name(ReporterId),
    gen_server:start_link(Name, ?MODULE, {ReporterId, Options}, []).

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------
%% @equiv start(ReporterId, [])
-spec start(reporter_id()) -> {ok, passage_reporter:reporter()} | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start(ReporterId) ->
    start(ReporterId, []).

%% @doc Starts a reporter process.
-spec start(reporter_id(), start_options()) -> {ok, Reporter} | {error, Reason} when
      Reporter :: passage_reporter:reporter(),
      Reason :: {already_started, pid()} | term().
start(ReporterId, Options) ->
    Args = [ReporterId, Options],
    is_atom(ReporterId) orelse error(badarg, Args),
    is_list(Options) orelse error(badarg, Args),

    case jaeger_passage_reporter_sup:start_child(ReporterId, Options) of
        {error, Reason} -> {error, Reason};
        {ok, _Pid}      -> {ok, passage_reporter:new(?MODULE, ReporterId)}
    end.

%% @doc Stops the reporter process.
%%
%% If the reporter which has the identifier `ReporterId' has not been started,
%% it will be simply ignored.
-spec stop(reporter_id()) -> ok.
stop(ReporterId) ->
    jaeger_passage_reporter_sup:stop_child(ReporterId).

%% @doc Returns the list of the running reporters.
-spec which_reporters() -> [reporter_id()].
which_reporters() ->
    jaeger_passage_reporter_sup:which_children().

%% @doc Returns the identifier of `Reporter'.
%%
%% Note that if `Reporter' is one which has not been created by {@link start/2},
%% this function will crash.
-spec get_id(passage_reporter:reporter()) -> reporter_id().
get_id(Reporter) ->
    ?MODULE = passage_reporter:get_module(Reporter),
    passage_reporter:get_state(Reporter).

%%------------------------------------------------------------------------------
%% 'passage_reporter' Callback Functions
%%------------------------------------------------------------------------------
%% @private
report(ReporterId, Span) ->
    Server = jaeger_passage_local_ns:reporter_name(ReporterId),
    gen_server:cast(Server, {report, Span}).

%%------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%------------------------------------------------------------------------------
%% @private
init({ReporterId, Options}) ->
    Format = proplists:get_value(thrift_format, Options, binary),
    DefaultPort =
        case Format of
            compact -> 6831;
            binary  -> 6832
        end,
    AgentHost = proplists:get_value(agent_host, Options, "127.0.0.1"),
    AgentPort = proplists:get_value(agent_port, Options, DefaultPort),
    DefaultServiceName = proplists:get_value(default_service_name, Options, ReporterId),
    Tags0 = proplists:get_value(process_tags, Options, #{}),

    {ok, Hostname} = inet:gethostname(),
    {ok, Version} = application:get_key(vsn),
    Tags1 =
        maps:merge(
          Tags0,
          #{
            ?JAEGER_CLIENT_VERSION_TAG_KEY => list_to_binary(["jaeger_passage-", Version]),
            ?TRACER_HOSTNAME_TAG_KEY => list_to_binary(Hostname),
            'erlang.node' => node()
           }),
    {ok, Client} = jaeger_passage_thrift:start_client(AgentHost, AgentPort, Format),
    State =
        #?STATE{
            thrift_client = Client,
            default_service_name  = DefaultServiceName,
            process_tags  = Tags1
           },
    {ok, State}.

%% @private
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @private
handle_cast({report, Span}, State) ->
    handle_report(Span, State);
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, State) ->
    jaeger_passage_thrift:stop_client(State#?STATE.thrift_client),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
-spec handle_report(passage_span:span(), #?STATE{}) -> {noreply, #?STATE{}}.
handle_report(Span, State) ->
    #?STATE{default_service_name = DefaultName, process_tags = Tags, thrift_client = Client} = State,
    Name = maps:get('location.application', passage_span:get_tags(Span), DefaultName),
    Client1 = jaeger_passage_thrift:send_report(Name, Tags, Span, Client),
    NewState = State#?STATE{thrift_client = Client1},
    {noreply, NewState}.
