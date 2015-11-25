-module(mqttserver_protocol).

-include("mqttserver.hrl").
-include("mqttserver_protocol.hrl").

%% API
-export([
    handle_message/2
]).


%% パケットに応じた処理を行う
handle_message(#type_connect{} = Message, State) ->
    io:format("CONNECT ~p~n", [Message]),
    {ok, State, [{response, mqttserver_parser:serialise(#type_connack{return_code=?CONNACK_ALLOW})}]};
handle_message(#type_publish{topic = Topic} = Message, State) ->
    io:format("PUBLISH ~p~n", [Message]),
    gproc:send({p, l, {subscriber, Topic}}, Message),
    {ok, State, []};
handle_message(#type_subscribe{message_id = MessageID,
                        topics = Topics} = Message, State) ->
    io:format("SUBSCRIBE ~p~n", [Message]),
    subscribe(Topics),
    {ok, State, [{response, mqttserver_parser:serialise(
        #type_suback{message_id = MessageID,
                     qoses = [Qos || {_Topic, Qos} <- Topics]}
    )}]};
handle_message(#type_unsubscribe{message_id = MessageID,
                          topics = Topics} = Message, State) ->
    io:format("UNSUBSCRIBE ~p~n", [Message]),
    unsubscribe(Topics),
    {ok, State, [{response, mqttserver_parser:serialise(
        #type_unsuback{message_id = MessageID}
    )}]};
handle_message(#type_disconnect{} = Message, State) ->
    io:format("DISCONNECT ~p~n", [Message]),
    {ok, State, []};
handle_message(Message, State) ->
    io:format("OTHER ~p~n", [Message]),
    {error, State, <<"ERROR">>}.


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
