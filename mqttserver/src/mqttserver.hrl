-include_lib("eunit/include/eunit.hrl").

-define(debugVal2(E),
    ((fun(__V) ->
        ?debugFmt(<<"~s = ~p">>, [(??E), __V]),
        __V
    end)(E))).


-define(TIMEOUT, 5000).

-type mode() :: waiting_for_connect | connected | disconnected.
-record(state, {socket :: inet:socket(),
    transport :: module(),
    buffer = <<>> :: binary(),
    mode = waiting_for_connect :: mode(),
    keep_alive = ?TIMEOUT :: non_neg_integer(),
    next_message_id = 1 :: non_neg_integer(),
    will = false :: boolean(),
    will_retain :: flag(),
    will_qos :: qos(),
    will_topic :: binary(),
    will_payload :: binary()}).
