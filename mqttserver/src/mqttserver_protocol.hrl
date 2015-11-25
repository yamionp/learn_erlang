
-define(CONNECT, 1).      % connect req from client
-define(CONNACK, 2).      % ack on connect from server
-define(PUBLISH, 3).      % message publish
-define(PUBACK, 4).       % ack on publish from server
-define(PUBREC, 5).       % part1
-define(PUBREL, 6).       % part2
-define(PUBCOMP, 7).      % part3
-define(SUBSCRIBE, 8).    % subscribe req from client
-define(SUBACK, 9).       % ack on subscribe from server
-define(UNSUBSCRIBE, 10). % unsubscribe req from client
-define(UNSUBACK, 11).    % ack on unsubscribe from server
-define(PINGREQ, 12).     % ping
-define(PINGRESP, 13).    % pong
-define(DISCONNECT, 14).  % client disconnecting
-define(DEFINENED, 15).   % defined by protocol

-define(TRUE, 1).
-define(FALSE, 0).

-define(CONNACK_ALLOW, 0).
-define(CONNACK_DENID_INVALID_VERSION, 1).
-define(CONNACK_DENID_INVALID_ID, 2).
-define(CONNACK_DENID_SERVER_DISABLE, 3).
-define(CONNACK_DENID_INVALID_AUTH, 4).
-define(CONNACK_DENID_PERMISSION, 5).

%% Fixed Header
-type flag() :: ?TRUE | ?FALSE.
-type qos() :: 0 | 1 | 2.

%% CONNECT Header
-type username() :: binary().
-type password() :: binary().
-type topic() :: binary().
-type client_id() :: binary().

-record(type_connect, {
    version :: 3 | 4,
    username :: username(),
    password :: password(),
    will_retain :: flag(),
    will_qos :: qos(),
    will_topic :: topic(),
    will_message :: binary(),
    clean_session :: flag(),
    keep_alive :: non_neg_integer(),
    client_id :: client_id()
}).
-type type_connect() :: #type_connect{}.

-record(type_connack, {
    return_code :: integer()
}).
-type type_connack() :: #type_connack{}.

-record(type_publish, {
    message_id = undefined :: undefined | non_neg_integer(),
    topic :: topic(),
    payload :: binary(),
    qos :: qos()
}).
-type type_publish() :: #type_publish{}.

-record(type_puback, {
    message_id :: non_neg_integer()
}).
-type type_puback() :: #type_puback{}.

-record(type_subscribe, {
    message_id :: non_neg_integer(),
    topics :: [{topic(), qos()}]
}).
-type type_subscribe() :: #type_subscribe{}.

-record(type_suback, {
    message_id :: non_neg_integer(),
    qoses :: [qos()]
}).
-type type_suback() :: #type_suback{}.

-record(type_unsubscribe, {
    message_id :: non_neg_integer(),
    topics :: [topic()]
}).
-type type_unsubscribe() :: #type_unsubscribe{}.

-record(type_unsuback, {
    message_id :: integer()
}).
-type type_unsuback() :: #type_unsuback{}.

-record(type_disconnect, {}).
-type type_disconnect() :: #type_disconnect{}.

-type type_message() :: type_connect()
    | type_connack()
    | type_publish()
    | type_puback()
    | type_subscribe()
    | type_suback()
    | type_unsubscribe()
    | type_unsuback()
    | type_disconnect().
