-module(calc).
-export([rpn/1, rpn_test/0]).

rpn(L) -> rpn_tail(string:tokens(L, " "), []).

rpn_tail([], Stack) -> hd(Stack);
rpn_tail([H|T], Stack) ->
    case H of
        "+" ->
            [Op1, Op2 | Rest] = Stack,
            rpn_tail(T, [to_n(Op2) + to_n(Op1) | Rest]);
        "-" ->
            [Op1, Op2 | Rest] = Stack,
            rpn_tail(T, [to_n(Op2) - to_n(Op1) | Rest]);
        "/" ->
            [Op1, Op2 | Rest] = Stack,
            rpn_tail(T, [to_n(Op2) / to_n(Op1) | Rest]);
        "*" ->
            [Op1, Op2 | Rest] = Stack,
            rpn_tail(T, [to_n(Op2) * to_n(Op1) | Rest]);
        "^" ->
            [Op1, Op2 | Rest] = Stack,
            rpn_tail(T, [math:pow(to_n(Op2), to_n(Op1)) | Rest]);
        "ln" ->
            [Op | Rest] = Stack,
            rpn_tail(T, [math:log(to_n(Op)) | Rest]);
        "log10" ->
            [Op | Rest] = Stack,
            rpn_tail(T, [math:log10(to_n(Op)) | Rest]);
        "sum" ->
            Op = lists:foldl(fun(E, Acc) -> to_n(E) + Acc end, 0, Stack),
            rpn_tail(T, [Op]);
        "prod" ->
            Op = lists:foldl(fun(E, Acc) -> to_n(E) * Acc end, 1, Stack),
            rpn_tail(T, [Op]);
        _ ->
            rpn_tail(T, [H | Stack])
    end.

to_n(S) when is_number(S) -> S;
to_n(S) ->
    case string:to_float(S) of
        {error,no_float} ->
            {IVal, _} = string:to_integer(S),
            IVal;
        {FVal, _} -> FVal
    end.

rpn_test() ->
    4 = rpn("1 3 +"),
    2.0 = rpn("4 2 /"),
    8 = rpn(" 4 2 *"),
    4 = rpn("6 2 -"),
    -4 = rpn("10 4 3 + 2 * -"),
    -2.0 = rpn("10 4 3 + 2 * - 2 /"),
    4037 = rpn("90 34 12 33 55 66 + * - + -"),
    8.0 = rpn("2 3 ^"),
    true = math:sqrt(2) == rpn("2 0.5 ^"),
    true = math:log(2.7) == rpn("2.7 ln"),
    true = math:log10(2.7) == rpn("2.7 log10"),
    50 = rpn("10 10 10 20 sum"),
    10.0 = rpn("10 10 10 20 sum 5 /"),
    1000.0 = rpn("10 10 20 0.5 prod"),
    ok.

