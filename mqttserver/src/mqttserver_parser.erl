-module(mqttserver_parser).

-include("mqttserver_protocol.hrl").

%% API
-export([
    parse/1,
    parse/2,
    serialise/1
]).


%% バイト列をパースしてMQTTのメッセージを切り出す
-spec parse(binary()) -> {more, binary()} | {ok, type_message(), binary()}.
parse(<<FixedHeader:1/binary, Tail/binary>> = Binary) when byte_size(Tail) > 0 ->
    {PayloadLength, Tail2} = calc_remain_length(Tail),
    case byte_size(Tail2) >= PayloadLength of
        true ->  %% 長さは足りている
            <<Payload:PayloadLength/binary, Rest/binary>> = Tail2,
            {ok, parse(FixedHeader, Payload), Rest};
        false ->  %% Payloadが足りない
            {more, Binary}
    end;
parse(Binary) ->  %% 固定ヘッダすらとれない
    {more, Binary}.


%% 残りのパケット長を計算する
-spec calc_remain_length(binary()) -> {non_neg_integer(), binary()}.
calc_remain_length(Message) ->
    calc_remain_length(Message, 0, 1).
calc_remain_length(<<0:1, Value:7, Rest/binary>>, TotalValue, Multiplier) ->
    TotalValue2 = TotalValue + Value * Multiplier,
    {TotalValue2, Rest};
calc_remain_length(<<1:1, Value:7, Rest/binary>>, TotalValue, Multiplier) ->
    TotalValue2 = TotalValue + Value * Multiplier,
    calc_remain_length(Rest, TotalValue2, Multiplier * 128).

%% MQTTのメッセージをRecordにする
-spec parse(binary(), binary()) -> type_message().
parse(<<?CONNECT:4, _Dup:1, _Qos:2, _Retain:1>>,
      <<  %% プロトコル情報
          ProtocolNameLength:16,
          _ProtocolName:ProtocolNameLength/binary,
          Ver:8,
          %% 接続フラグ開始
          UsernameFlg:1,
          PasswordFlg:1,
          WillRetain:1,
          WillQos:2,
          WillFlg:1,
          CleanSession:1,
          _:1, %% 予約ビット
          %% 接続フラグここまで
          KeepAlive:16,
          ClientIdLength:16,
          ClientId:ClientIdLength/binary,
          Payload0/binary>>) ->
    Message0 = #type_connect{
        version = Ver,
        will_retain = WillRetain,
        will_qos = WillQos,
        clean_session = CleanSession,
        keep_alive = KeepAlive,
        client_id = ClientId
    },
    {Payload1, Message1} = parse_will(Payload0, WillFlg, Message0),
    {Payload2, Message2} = parse_username(Payload1, UsernameFlg, Message1),
    {_, Message3} = parse_password(Payload2, PasswordFlg, Message2),
    Message3;
parse(<<?PUBLISH:4, _Dup:1, _Qos:2, _Retain:1>>,
      <<TopicLength:16, Topic:TopicLength/binary, Payload/binary>>) ->
    #type_publish{
        topic = Topic,
        payload = Payload
    };
parse(<<?SUBSCRIBE:4, _Dup:1, _Qos:2, _Retain:1>>,
      <<MessageID:16, Topics/binary>>) ->
    case parse_qos_topics(Topics, []) of
        {ok, ParsedTopic} ->
            #type_subscribe{
                message_id = MessageID,
                topics = ParsedTopic
            };
        E -> E
    end;
parse(<<?UNSUBSCRIBE:4, _Dup:1, _Qos:2, _Retain:1>>,
      <<MessageID:16, Topics/binary>>) ->
    case parse_topics(Topics, []) of
        {ok, ParsedTopic} ->
            #type_unsubscribe{
                message_id = MessageID,
                topics = ParsedTopic
            };
        E -> E
    end;
parse(<<?DISCONNECT:4, _Dup:1, _Qos:2, _Retain:1>>, <<>>) ->
    #type_disconnect{};
parse(_, _) -> {error, cant_parse}.


%% CONNECTのPayloadからWillを取り出す
-spec parse_will(binary(), non_neg_integer(), type_connect()) -> {binary(), type_connect()}.
parse_will(Binary, 0, Message) -> {Binary, Message};
parse_will(<<WillTopicLength:16,
             WillTopic:WillTopicLength/binary,
             WillMessageLength:16,
             WillMessage:WillMessageLength/binary,
             Remain/binary>>, 1, Message) ->
    {Remain, Message#type_connect{
        will_topic = WillTopic,
        will_message = WillMessage
    }}.

%% CONNECTのPayloadからUsernameを取り出す
-spec parse_username(binary(), non_neg_integer(), type_connect()) -> {binary(), type_connect()}.
parse_username(Binary, 0, Message) -> {Binary, Message};
parse_username(<<UsernameLength:16,
                 Username:UsernameLength/binary,
                 Remain/binary>>, 1, Message) ->
    {Remain, Message#type_connect{username = Username}}.

%% CONNECTのPayloadからPasswordを取り出す
-spec parse_password(binary(), non_neg_integer(), type_connect()) -> {binary(), type_connect()}.
parse_password(Binary, 0, Message) -> {Binary, Message};
parse_password(<<PasswordLength:16,
                 Password:PasswordLength/binary,
                 Remain/binary>>, 1, Message) ->
    {Remain, Message#type_connect{password = Password}}.

%% 連続したTopicをリストにして返す
-spec parse_topics(binary(), [topic()]) -> {error, no_topic} | {ok, [topic()]}.
parse_topics(<<>>, []) ->
    {error, no_topic};
parse_topics(<<>>, Topics) ->
    {ok, Topics};
parse_topics(<<TopicLength:16, Topic:TopicLength/binary,
               Rest/binary>>, Topics) ->
    parse_topics(Rest, [Topic | Topics]).
%% 連続したTopic,Qosをタプルのリストにして返す
-spec parse_qos_topics(binary(), [{topic(), qos()}]) -> {error, no_topic} | {ok, [{topic(), qos()}]}.
parse_qos_topics(<<>>, []) ->
    {error, no_topic};
parse_qos_topics(<<>>, Topics) ->
    {ok, Topics};
parse_qos_topics(<<TopicLength:16, Topic:TopicLength/binary,
                   _:6, %% QosPadding
                   Qos:2/integer,
                   Rest/binary>>, Topics) ->
    parse_qos_topics(Rest, [{Topic, Qos} | Topics]).


-spec serialise(type_message()) -> iolist().
serialise(#type_publish{topic=Topic,
                        payload=Payload}) ->
    Var = [utf8(Topic), Payload],
    LenBytes = serialise_len(iolist_size(Var)),
    [<<?PUBLISH:4, 0:4/integer>>, LenBytes, Var];
serialise(#type_suback{message_id=MessageId, qoses=Qoses}) ->
    SerialisedAcks = serialise_acks(Qoses, []),
    LenBytes = serialise_len(iolist_size(SerialisedAcks) + 2),
    [<<?SUBACK:4, 0:4>>, LenBytes, <<MessageId:16>>, SerialisedAcks];
serialise(#type_unsuback{message_id=MessageId}) ->
    [<<?UNSUBACK:4, 0:4>>, serialise_len(2), <<MessageId:16>>];
serialise(#type_connack{return_code=RC}) ->
    [<<?CONNACK:4, 0:4>>, serialise_len(2), <<RC:16>>].

-spec utf8(binary()) -> binary().
utf8(Bin) when is_binary(Bin) ->
    <<(byte_size(Bin)):16, Bin/binary>>.

-spec serialise_len(non_neg_integer()) -> binary().
serialise_len(N) when N =< 127 ->
    <<0:1, N:7>>;
serialise_len(N) ->
    <<1:1, (N rem 128):7, (serialise_len(N div 128))/binary>>.

-spec serialise_acks([qos()], list()) -> [binary()].
serialise_acks([], Acks) ->
    Acks;
serialise_acks([QoS|Rest], Acks) ->
    serialise_acks(Rest, [<<0:6, QoS:2>>|Acks]).
