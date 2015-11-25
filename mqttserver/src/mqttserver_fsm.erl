-module(mqttserver_fsm).

-include("mqttserver.hrl").
-include("mqttserver_protocol.hrl").

%% API
-export([
    waiting_for_connect/2,
    connected/2
]).


%% CONNECT待ち
waiting_for_connect(#type_connect{keep_alive = KeepAlive} = Message, State) ->
    io:format("waiting_for_connect: CONNECT ~p~n", [Message]),
    {ok, State#state{mode = connected,
                     keep_alive = KeepAlive * 1000},
        [#type_connack{return_code=?CONNACK_ALLOW}]};
waiting_for_connect(Message, State) ->
    io:format("waiting_for_connect: OTHER ~p~n", [Message]),
    {error, State, <<"ERROR">>}.

%% CONNECT済み

%% PUBLISH QoS0
connected(#type_publish{topic = Topic,
                        qos = 0} = Message, State) ->
    io:format("PUBLISH ~p~n", [Message]),
    gproc:send({p, l, {subscriber, Topic}}, Message),
    {ok, State, []};
%% PUBLISH QoS1
connected(#type_publish{topic = Topic,
                        qos = 1,
                        message_id = MessageID} = Message, State) ->
    io:format("PUBLISH ~p~n", [Message]),
    gproc:send({p, l, {subscriber, Topic}}, Message),
    {ok, State, [#type_puback{message_id = MessageID}]};
connected(#type_subscribe{message_id = MessageID,
                        topics = Topics} = Message, State) ->
    io:format("SUBSCRIBE ~p~n", [Message]),
    subscribe(Topics),
    {ok, State, [
        #type_suback{message_id = MessageID,
                     qoses = [Qos || {_Topic, Qos} <- Topics]}
    ]};
connected(#type_unsubscribe{message_id = MessageID,
                          topics = Topics} = Message, State) ->
    io:format("UNSUBSCRIBE ~p~n", [Message]),
    unsubscribe(Topics),
    {ok, State, [#type_unsuback{message_id = MessageID}]};
connected(#type_disconnect{} = Message, State) ->
    io:format("DISCONNECT ~p~n", [Message]),
    {ok, State#state{mode = waiting_for_connect}, []};
connected(#type_puback{} = Message, State) ->
    io:format("PUBACK ~p~n", [Message]),
    {ok, State, []};
connected(Message, State) ->
    io:format("connected: OTHER ~p~n", [Message]),
    {error, State, <<"ERROR">>}.

%% inner functions
subscribe([]) ->
    ok;
subscribe([{Topic, _Qos}|Rest]) ->
    true = gproc:reg({p, l, {subscriber, Topic}}, undefined),
    ?debugVal2(gproc:lookup_local_properties({subscriber, Topic})),
    subscribe(Rest).

unsubscribe([]) ->
    ok;
unsubscribe([Topic|Rest]) ->
    gproc:unreg({p, l, {subscriber, Topic}}),
    ?debugVal2(gproc:lookup_local_properties({subscriber, Topic})),
    unsubscribe(Rest).
