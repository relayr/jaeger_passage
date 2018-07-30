%%% @copyright (C) 2018, Relayr
-module(jaeger_passage_thrift_transport).
-author("pbober").

-behaviour(thrift_transport).

-include_lib("thrift/include/thrift_transport_behaviour.hrl").

-export([new_transport_factory/3]).
-export([read/2, write/2, flush/1, close/1]). % callbacks

-record(state, {
    socket :: gen_udp:socket(),
    host :: inet:hostname(),
    port :: inet:port_number(),
    recv_timeout :: timeout()
}).
-type state() :: #state{}.

-type options() :: #{
    recv_timeout => timeout(),
    sock_opts => [any()]
}.

-define(DEFAULT_RECV_TIMEOUT, 5000).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

-spec new_transport_factory(inet:hostname(), inet:port_number(), options()) -> {ok, function()}.
new_transport_factory(Host, Port, Options) ->
    F = fun() ->
            SockOpts = maps:get(sock_opts, Options, []),
            RecvTimeout = maps:get(recv_timeout, Options, ?DEFAULT_RECV_TIMEOUT),
            case catch gen_udp:open(0, SockOpts) of
                {ok, Socket} ->
                    State = #state{socket = Socket, host = Host, port = Port, recv_timeout = RecvTimeout},
                    {ok, BaseTransport} = thrift_transport:new(?MODULE, State),
                    thrift_buffered_transport:new(BaseTransport);
                Error ->
                    Error
            end
        end,
    {ok, F}.

%%------------------------------------------------------------------------------
%% thrift_transport callbacks
%%------------------------------------------------------------------------------

write(State = #state{socket = Socket, host = Host, port = Port}, Data) ->
    {State, gen_udp:send(Socket, Host, Port, Data)}.

read(State = #state{socket=Socket, recv_timeout = Timeout}, Len) when is_integer(Len), Len >= 0 ->
    case gen_udp:recv(Socket, Len, Timeout) of
        {ok, {_Host, _Port, Data}} ->
            {State, {ok, Data}};
        {error, _} = Error ->
            Error
    end.

flush(State) ->
    % nothing to flush
    {State, ok}.

close(State = #state{socket = Socket}) ->
    {State, gen_udp:close(Socket)}.
