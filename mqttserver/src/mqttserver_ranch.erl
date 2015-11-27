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

%% データ受信
handle_info({tcp, Socket, Data},
    #state{socket=Socket,
           transport=Transport,
           buffer=Buffer} = State) ->
    Transport:setopts(Socket, [{active, once}]),
    case handle_binary(<<Buffer/binary, Data/binary>>, State) of
        {NewState, Rest} ->
            {noreply, NewState#state{buffer = Rest}, NewState#state.keep_alive};
        {error, NewState, Reason, Rest} ->
            ?debugVal(Reason),
            {noreply, NewState#state{buffer = Rest}, NewState#state.keep_alive}
    end;
%% 切断 connected かつwill有り
handle_info({tcp_closed, Socket},
            #state{mode = connected,
                   will = true,
                   will_qos = QoS,
                   will_topic = Topic,
                   will_payload = Payload} = State) ->
    ?debugVal({tcp_closed, Socket, State}),
    %% Todo: ここにコレを書きたくないのでFSMを別プロセスにし、メッセージで飛ばすように変える
    gproc:send({p, l, {subscriber, Topic}},
               #type_publish{qos = QoS,
                             topic = Topic,
                             payload = Payload}),
    {stop, normal, State};
%% 切断 will無し
handle_info({tcp_closed, Socket},
            #state{will = false} = State) ->
    ?debugVal({tcp_closed, Socket, State}),
    {stop, normal, State};
%% 接続エラー
handle_info({tcp_error, _, Reason}, State) ->
    ?debugVal({tcp_error, Reason}),
    {stop, Reason, State};
%% タイムアウト
handle_info(timeout, State) ->
    {stop, normal, State};
%% publishが回ってきた
handle_info(#type_publish{qos = 0} = Message,
            #state{socket = Socket,
                   transport = Transport,
                   keep_alive = Timeout} = State) ->
    ok = Transport:send(Socket, mqttserver_parser:serialise(Message)),
    {noreply, State, Timeout};
%% publish qos 1以上 なのでMessageIDをつけて送信
handle_info(#type_publish{} = Message,
            #state{socket = Socket,
                   transport = Transport,
                   keep_alive = Timeout,
                   next_message_id = NextMessageID} = State) ->
    ok = Transport:send(Socket, mqttserver_parser:serialise(Message#type_publish{message_id = NextMessageID})),
    {noreply, State#state{next_message_id = NextMessageID + 1}, Timeout};
handle_info(Info, State) ->
    ?debugVal(Info),
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% バイナリを受け取って処理する
handle_binary(Data, #state{mode=Mode} = State) ->
    case mqttserver_parser:parse(Data) of
        {more, Binary} ->
            {State, Binary};
        {ok, Message, Binary} ->
            %% vmqっぽくしたが、やはり別プロセスでメッセージパッシングしたほうがよさそう
            case mqttserver_fsm:Mode(Message, State) of
                {ok, NewState, Outs} ->
                    flush(Outs, NewState),
                    handle_binary(Binary, NewState);
                {error, NewState, Reason} ->
                    {error, NewState, Reason, Binary}
            end;
        {error, Reason, Binary} ->
            {error, State, Reason, Binary}
    end.

%% メッセージを送信する
flush([], _) ->
    ok;
flush([Message],
      #state{socket = Socket, transport = Transport}) ->
    Transport:send(Socket, mqttserver_parser:serialise(Message));
flush([Message|Rest],
      #state{socket = Socket, transport = Transport} = State) ->
    ok = Transport:send(Socket, mqttserver_parser:serialise(Message)),
    flush(Rest, State).
