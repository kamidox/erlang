-module(useless).
-export([add/2, hello/0, greet_and_add_two/1]).

add(A, B) ->
    A + B.

%% comments come here start with one %
hello() ->
    io:format("hello word!~n~n").

greet_and_add_two(X) ->
    hello(),
    add(X, 2).

