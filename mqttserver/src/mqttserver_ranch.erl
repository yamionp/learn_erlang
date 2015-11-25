-module(mqttserver_ranch).

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


handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({tcp, Socket, Data},
    #state{socket=Socket,
           transport=Transport,
           buffer=Buffer} = State) ->
    Transport:setopts(Socket, [{active, once}]),
    case handle_binary(<<Buffer/binary, Data/binary>>, State) of
        {NewState, Rest} ->
            {noreply, NewState#state{buffer = Rest}, NewState#state.timeout};
        {error, NewState, Reason, Rest} ->
            ?debugVal(Reason),
            {noreply, NewState#state{buffer = Rest}, NewState#state.timeout}
    end;
handle_info({tcp_closed, Socket}, State) ->
    ?debugVal({tcp_closed, Socket}),
    {stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
    ?debugVal({tcp_error, Reason}),
    {stop, Reason, State};
handle_info(timeout, State) ->
    {stop, normal, State};
handle_info(#type_publish{} = Message,
            #state{socket = Socket,
                   transport = Transport,
                   timeout = Timeout} = State) ->
    ok = Transport:send(Socket, mqttserver_parser:serialise(Message)),
    {noreply, State, Timeout};
handle_info(_Info, State) ->
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_binary(Data, #state{mode=Mode} = State) ->
    case mqttserver_parser:parse(Data) of
        {more, Binary} ->
            {State, Binary};
        {ok, Message, Binary} ->
            case mqttserver_protocol:Mode(Message, State) of
                {ok, NewState, Outs} ->
                    ?debugVal(NewState),
                    ?debugVal(Outs),
                    flush(Outs, NewState),
                    handle_binary(Binary, NewState);
                {error, NewState, Reason} ->
                    {error, NewState, Reason, Binary}
            end
    end.

flush([], _) ->
    ok;
flush([{response, Binary}|Rest],
      #state{socket = Socket, transport = Transport} = State) ->
    ok = Transport:send(Socket, Binary),
    flush(Rest, State).
