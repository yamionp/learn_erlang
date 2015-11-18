-include_lib("eunit/include/eunit.hrl").

-define(debugVal2(E),
    ((fun(__V) ->
        ?debugFmt(<<"~s = ~p">>, [(??E), __V]),
        __V
    end)(E))).
