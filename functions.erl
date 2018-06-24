-module(functions).
-compile(export_all).

head([X|_]) ->
    X.

second([_,X|_]) ->
    X.

right_age(X) when X >= 16, X < 104 -> true;
right_age(_) -> false.

wrong_age(X) when X < 16; X >= 104 -> true;
wrong_age(_) -> false.

