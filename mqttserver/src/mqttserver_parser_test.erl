-module(mqttserver_parser_test).
-include_lib("eunit/include/eunit.hrl").
-include("mqttserver_protocol.hrl").


parse_connect_test() ->
    %% 正常データセット
    Packet = <<16, 35, 0, 4, 77, 81, 84, 84, 4, 2, 0, 60, 0, 23, 112, 97, 104, 111, 47, 55, 51, 68, 57, 65, 69, 70, 55, 50, 67, 57, 53, 56, 49, 49, 51, 51, 67>>,
    Message = #type_connect{version = 4, will_retain = 0, will_qos = 0, clean_session = 1, keep_alive = 60, client_id = <<"paho/73D9AEF72C9581133C">>},

    %% 正常系
    ?assertEqual(
        {ok, Message, <<>>},
        mqttserver_parser:parse(Packet)
    ),
    %% 余分なバイナリが続いている
    ?assertEqual(
        {ok, Message, <<224, 0>>},
        mqttserver_parser:parse(<<Packet/binary, 224, 0>>)
    ),

    %% 空バイナリ
    ?assertEqual(
        {more, <<>>},
        mqttserver_parser:parse(<<>>)
    ),
    %% Lengthが無い
    <<Head1:1/binary, _/binary>> = Packet,
    ?assertEqual(
        {more, Head1},
        mqttserver_parser:parse(Head1)
    ),
    %% 固定ヘッダ、Lengthのみ
    <<Head2:2/binary, _/binary>> = Packet,
    ?assertEqual(
        {more, Head2},
        mqttserver_parser:parse(Head2)
    ),
    %% Payloadの長さが足りてないので続きを待つ
    <<Head3:3/binary, _/binary>> = Packet,
    ?assertEqual(
        {more, Head3},
        mqttserver_parser:parse(Head3)
    ).

parse_disconnect_test() ->
    %% 正常データセット
    Packet = <<224, 0>>,
    Message = #type_disconnect{},

    %% 正常系
    ?assertEqual(
        {ok, Message, <<>>},
        mqttserver_parser:parse(Packet)
    ),
    %% 余分なバイナリが続いている
    ?assertEqual(
        {ok, Message, <<224, 0>>},
        mqttserver_parser:parse(<<Packet/binary, 224, 0>>)
    ),

    %% 空バイナリ
    ?assertEqual(
        {more, <<>>},
        mqttserver_parser:parse(<<>>)
    ),
    %% Lengthが無い
    <<Head1:1/binary, _/binary>> = Packet,
    ?assertEqual(
        {more, Head1},
        mqttserver_parser:parse(Head1)
    ).


parse_publish_test() ->
    %% 正常データセット
    Packet = <<48, 26, 0, 10, 115, 97, 109, 112, 108, 101, 47, 97, 98, 99, 48, 58, 116, 101, 115, 116, 32, 109, 101, 115, 115, 97, 103, 101>>,
    Message = #type_publish{topic = <<"sample/abc">>, payload = <<"0:test message">>},

    %% 正常系
    ?assertEqual(
        {ok, Message, <<>>},
        mqttserver_parser:parse(Packet)
    ),
    %% 余分なバイナリが続いている
    ?assertEqual(
        {ok, Message, <<224, 0>>},
        mqttserver_parser:parse(<<Packet/binary, 224, 0>>)
    ),

    %% 空バイナリ
    ?assertEqual(
        {more, <<>>},
        mqttserver_parser:parse(<<>>)
    ),
    %% Lengthが無い
    <<Head1:1/binary, _/binary>> = Packet,
    ?assertEqual(
        {more, Head1},
        mqttserver_parser:parse(Head1)
    ),
    %% 固定ヘッダ、Lengthのみ
    <<Head2:2/binary, _/binary>> = Packet,
    ?assertEqual(
        {more, Head2},
        mqttserver_parser:parse(Head2)
    ),
    %% Payloadの長さが足りてないので続きを待つ
    <<Head3:3/binary, _/binary>> = Packet,
    ?assertEqual(
        {more, Head3},
        mqttserver_parser:parse(Head3)
    ).

parse_subscribe_test() ->
    %% 正常データセット
    Packet = <<130,27,0,1,0,10,115,97,109,112,108,101,47,97,98,99,0,0,3,97,47,98,1,0, 3,99,47,100,2>>,
    Message = #type_subscribe{message_id = 1, topics = [{<<"c/d">>, 2}, {<<"a/b">>, 1}, {<<"sample/abc">>, 0}]},

    %% 正常系
    ?assertEqual(
        {ok, Message, <<>>},
        mqttserver_parser:parse(Packet)
    ),
    %% 余分なバイナリが続いている
    ?assertEqual(
        {ok, Message, <<224, 0>>},
        mqttserver_parser:parse(<<Packet/binary, 224, 0>>)
    ),

    %% 空バイナリ
    ?assertEqual(
        {more, <<>>},
        mqttserver_parser:parse(<<>>)
    ),
    %% Lengthが無い
    <<Head1:1/binary, _/binary>> = Packet,
    ?assertEqual(
        {more, Head1},
        mqttserver_parser:parse(Head1)
    ),
    %% 固定ヘッダ、Lengthのみ
    <<Head2:2/binary, _/binary>> = Packet,
    ?assertEqual(
        {more, Head2},
        mqttserver_parser:parse(Head2)
    ),
    %% Payloadの長さが足りてないので続きを待つ
    <<Head3:3/binary, _/binary>> = Packet,
    ?assertEqual(
        {more, Head3},
        mqttserver_parser:parse(Head3)
    ).

parse_unsubscribe_test() ->
    %% 正常データセット
    Packet = <<162, 12, 0, 2, 0, 3, 97, 47, 98, 0, 3, 99, 47, 100>>,
    Message = #type_unsubscribe{message_id = 2, topics = [<<"c/d">>, <<"a/b">>]},

    %% 正常系
    ?assertEqual(
        {ok, Message, <<>>},
        mqttserver_parser:parse(Packet)
    ),
    %% 余分なバイナリが続いている
    ?assertEqual(
        {ok, Message, <<224, 0>>},
        mqttserver_parser:parse(<<Packet/binary, 224, 0>>)
    ),

    %% 空バイナリ
    ?assertEqual(
        {more, <<>>},
        mqttserver_parser:parse(<<>>)
    ),
    %% Lengthが無い
    <<Head1:1/binary, _/binary>> = Packet,
    ?assertEqual(
        {more, Head1},
        mqttserver_parser:parse(Head1)
    ),
    %% 固定ヘッダ、Lengthのみ
    <<Head2:2/binary, _/binary>> = Packet,
    ?assertEqual(
        {more, Head2},
        mqttserver_parser:parse(Head2)
    ),
    %% Payloadの長さが足りてないので続きを待つ
    <<Head3:3/binary, _/binary>> = Packet,
    ?assertEqual(
        {more, Head3},
        mqttserver_parser:parse(Head3)
    ).
