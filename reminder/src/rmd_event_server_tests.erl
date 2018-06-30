-module(rmd_event_server_tests).
-include_lib("eunit/include/eunit.hrl").

rmd_event_server_test_() ->
    {
        foreach,
        fun start/0,
        fun stop/1,
        [
            fun subscribe_error/1,
            fun subscribe_ok/1,
            fun add_event_error/1,
            fun add_event_invalid/1
            % fun add_event_ok/1
        ]
    }.

subscribe_error(_) ->
    rmd_event_server:terminate(),
    [?_assertEqual({error, already_terminated}, rmd_event_server:subscribe(self()))].

subscribe_ok(_) ->
    {Result, _} = rmd_event_server:subscribe(self()),
    rmd_event_server:terminate(),
    [?_assertEqual(Result, ok)].

add_event_error(_) ->
    rmd_event_server:terminate(),
    [?_assertEqual({error, already_terminated}, rmd_event_server:add_event("Test", "Test Desc", 3))].

add_event_invalid(_) ->
    [?_assertEqual({error, bad_timeout}, rmd_event_server:add_event("Test", "Test Desc", bad))].

add_event_ok(_) ->
    rmd_event_server:subscribe(self()),
    Result = rmd_event_server:add_event("Test", "Test Desc", 1),
    receive
        Msg -> [?_assertEqual({done, "Test", "Test Desc"}, Msg),
                ?_assertEqual(Result, ok)]
    after 2000 ->
        [?_assert(false)]
    end.

start() ->
    rmd_event_server:start().

stop(_) ->
    rmd_event_server:terminate().

