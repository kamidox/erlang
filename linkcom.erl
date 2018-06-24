-module(linkcom).
-export([myproc/0, critic/0, judge/1, restarter/0, start_critic/0]).

myproc() ->
    timer:sleep(5000),
    exit(reason).

critic() ->
    receive
        {From, Ref, book} ->
            From ! {Ref, "Good idea!"};
        {From, Ref, movie} ->
            From ! {Ref, "Great, let's do it!"};
        {From, Ref, _} ->
            From ! {Ref, "eh... I would like to say no."}
    end,
    critic().

judge(Act) ->
    Ref = make_ref(),
    critic ! {self(), Ref, Act},
    receive
        {Ref, Criticism} -> Criticism
    after 2000 ->
        timeout
    end.

restarter() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, critic, []),
    register(critic, Pid),
    receive
        {'EXIT', Pid, normal} ->
            io:format("critic exit normal.~n"),
            ok;
        {'EXIT', Pid, shutdown} ->
            io:format("critic shutdown.~n"),
            ok;
        {'EXIT', Pid, _} ->
            io:format("critic exit unexpected, restarting ..."),
            restarter()
    end.

start_critic() ->
    spawn(?MODULE, restarter, []).

