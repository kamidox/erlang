-module(demo_tests).
-include_lib("eunit/include/eunit.hrl").

add_test_() ->
    [?_assert(4 =:= demo:add(2, 2)),
     ?_assertEqual(3, demo:add(2, 2)),
     ?_assertEqual(5, demo:add(2, 3))].

