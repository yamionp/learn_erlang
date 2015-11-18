-module(mqttserver_protocol).

-include("mqttserver.hrl").
-include("mqttserver_protocol.hrl").

%% API
-export([
    command/1
]).


%% パケットに応じた処理を行う
command(#type_connect{} = Message) ->
    io:format("CONNECT ~p~n", [Message]),
    {ok, [{response, mqttserver_parser:serialise(#type_connack{return_code=?CONNACK_ALLOW})}]};
command(#type_publish{topic = Topic} = Message) ->
    io:format("PUBLISH ~p~n", [Message]),
    ?debugVal(Topic),
    ?debugVal2(gproc:lookup_local_properties({subscriber, Topic})),
    gproc:send({p, l, {subscriber, Topic}}, {publish, Message}),
    {ok, []};
command(#type_subscribe{message_id = MessageID,
                        topics = Topics} = Message) ->
    io:format("SUBSCRIBE ~p~n", [Message]),
    subscribe(Topics),
    {ok, [{response, mqttserver_parser:serialise(
        #type_suback{message_id = MessageID,
                     qoses = [Qos || {_Topic, Qos} <- Topics]}
    )}]};
command(#type_unsubscribe{message_id = MessageID,
                          topics = Topics} = Message) ->
    io:format("UNSUBSCRIBE ~p~n", [Message]),
    unsubscribe(Topics),
    {ok, [{response, mqttserver_parser:serialise(
        #type_unsuback{message_id = MessageID}
    )}]};
command(#type_disconnect{} = Message) ->
    io:format("DISCONNECT ~p~n", [Message]),
    {ok, []};
command(Message) ->
    io:format("OTHER ~p~n", [Message]),
    {error, <<"ERROR">>}.


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
