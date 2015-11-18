-module(mqttserver_ranch).
-author("yugo.shimizu").

-behaviour(gen_server).
-behaviour(ranch_protocol).

%% ranch
-export([start_link/4]).
-export([init/4]).

%% gen_server
-export([init/1]).
-export([handle_info/2]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([terminate/2]).
-export([code_change/3]).

-include("mqttserver_protocol.hrl").
-include("mqttserver.hrl").

-define(TIMEOUT, 5000).

-record(state, {socket :: inet:socket(),
                transport :: module(),
                buffer = <<>> :: binary()}).


start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).


init([]) -> {ok, undefined}.
init(Ref, Socket, Transport, _Opts = []) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    gen_server:enter_loop(?MODULE, [],
                          #state{
                              socket=Socket,
                              transport=Transport
                          }, ?TIMEOUT).

handle_info({tcp, Socket, Data},
    State=#state{socket=Socket,
        transport=Transport,
        buffer=Buffer}) ->
    Transport:setopts(Socket, [{active, once}]),
    case handle_binary(<<Buffer/binary, Data/binary>>, State) of
        {State, Rest} ->
            {noreply, State#state{buffer = Rest}, ?TIMEOUT};
        {error, Reason, Rest} ->
            ?debugVal(Reason),
            {noreply, State#state{buffer = Rest}, ?TIMEOUT}
    end;
handle_info({tcp_closed, _Socket}, State) ->
    ?debugVal(_Socket),
    {stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
    ?debugVal(Reason),
    {stop, Reason, State};
handle_info(timeout, State) ->
    ?debugVal(State),
    {stop, normal, State};
handle_info({publish, #type_publish{} = Message},
            #state{socket = Socket, transport = Transport} = State) ->
    ?debugVal(Message),
    ok = Transport:send(Socket, mqttserver_parser:serialise(Message)),
    {noreply, State, ?TIMEOUT};
handle_info(Info, State) ->
    ?debugVal(Info),
    ?debugVal(State),
    {stop, normal, State}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_binary(Data, State) ->
    case mqttserver_parser:parse(Data) of
        {more, Binary} ->
            {State, Binary};
        {ok, Message, Binary} ->
            case mqttserver_protocol:command(Message) of
                {ok, Outs} ->
                    flush(Outs, State),
                    handle_binary(Binary, State);
                {error, Reason} ->
                    {error, Reason, Binary}
            end
    end.

flush([], _) ->
    ok;
flush([{response, Binary}|Rest],
      #state{socket = Socket, transport = Transport} = State) ->
    ok = Transport:send(Socket, Binary),
    flush(Rest, State).
