%%%-------------------------------------------------------------------
%%% @author kamidox@qq.com
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Jun 2018 19:41
%%%-------------------------------------------------------------------
-module(demo).
-author("kamidox@qq.com").

%% API
-export([add/2, double/1, hello/0]).

add(X, Y) ->
  X + Y.

double(L) ->
  [X * 2 || X <- L].

hello() ->
    useless:hello(),
    io:format("hello word.~n").

