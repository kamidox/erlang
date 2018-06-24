-module(rmd_event_tests).
-include_lib("eunit/include/eunit.hrl").

event_test() ->
    rmd_event:start("Test", 0),
    receive
        {done, Name} -> ?assertEqual(Name, "Test")
    after 100 ->
        ?assert(false)
    end.

cancel_test() ->
    Pid = rmd_event:start("Test", 0.1),
    ?assertEqual(ok, rmd_event:cancel(Pid)),
    receive
        {done, _} -> ?assert(false)
    after 200 ->
        ?assert(true)
    end.

