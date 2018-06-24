-module(dolphine).
-compile(export_all).

dolphine1() ->
    receive
        {From, do_a_flip} ->
            From ! "How about no?",
            dolphine1();
        {From, fish} ->
            From ! "Thanks for the fish, bye ~~";
        _ ->
            io:format("Heh, we are smarter than humans~n"),
            dolphine1()
    end.


