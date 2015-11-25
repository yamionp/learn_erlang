-include_lib("eunit/include/eunit.hrl").

-define(debugVal2(E),
    ((fun(__V) ->
        ?debugFmt(<<"~s = ~p">>, [(??E), __V]),
        __V
    end)(E))).


-define(TIMEOUT, 5000).

-type mode() :: waiting_for_connect | connected.
-record(state, {socket :: inet:socket(),
    transport :: module(),
    buffer = <<>> :: binary(),
    mode = waiting_for_connect :: mode(),
    keep_alive = ?TIMEOUT :: non_neg_integer(),
    will :: boolean(),
    will_topic :: binary(),
    will_payload :: binary()}).
