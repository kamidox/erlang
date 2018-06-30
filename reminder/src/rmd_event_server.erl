-module(rmd_event_server).
-export([init/0, loop/1, subscribe/1, start/0, start_link/0, terminate/0,
        add_event/3, hotload/0]).
-record(event, {
    name="",
    desc="",
    pid,
    timeout
}).
-record(state, {
    events,         %% list of #event{} record
    clients         %% list of client pid
}).

valid_datetime(T) when is_number(T) ->
    if T >= 0 -> true;
        T < 0 -> false
    end;
valid_datetime(_) -> false.

send_to_client(Msg, Clients) ->
    io:format("send ~p to ~p clients.~n", [Msg, dict:size(Clients)]),
    dict:map(fun(_, Client) -> Client ! Msg end, Clients).

loop(S) ->
    receive
        {Pid, MsgRef, {subscribe, Client}} ->
            %% new client
            Ref = erlang:monitor(process, Client),
            NewClients = dict:store(Ref, Client, S#state.clients),
            Pid ! {MsgRef, ok},
            loop(S#state{clients=NewClients});
        {'DOWN', Ref, process, _Pid, _Reason} ->
            %% client exit
            NewClients = dict:erase(Ref, S#state.clients),
            loop(S#state{clients=NewClients});
        {Pid, MsgRef, {add, Name, Desc, Timeout}} ->
            %% add new event
            case valid_datetime(Timeout) of
                true ->
                    EventPid = rmd_event:start(Name, Timeout),
                    NewEvents = dict:store(Name,
                                           #event{name=Name,
                                                  desc=Desc,
                                                  pid=EventPid,
                                                  timeout=Timeout},
                                           S#state.events),
                    Pid ! {MsgRef, ok},
                    loop(S#state{events=NewEvents});
                false ->
                    Pid ! {MsgRef, {error, bad_timeout}},
                    loop(S)
                end;
        {Pid, MsgRef, {cancel, Name}} ->
            %% cancel event by Name
            NewEvents = case dict:find(Name, S#state.events) of
                            {ok, E} ->
                                rmd_event:cancel(E#event.pid),
                                dict:erase(Name, S#state.events);
                            error ->
                                S#state.events
                        end,
            Pid ! {MsgRef, ok},
            loop(S#state{events=NewEvents});
        {done, Name} ->
            %% receive done event from rmd_event
            io:format("Event ~p done.~n", [Name]),
            case dict:find(Name, S#state.events) of
                {ok, E} ->
                    send_to_client({done, E#event.name, E#event.desc}, S#state.clients),
                    NewEvents = dict:erase(Name, S#state.events),
                    loop(S#state{events=NewEvents});
                error ->
                    %% race condition: we cancel the event and the event timeout in the same time
                    loop(S)
            end;
        shutdown ->
            %% client request to shutdown event server
            exit(shutdown);
        code_change ->
            %% load new version
            ?MODULE:loop(S);
        Unknown ->
            io:format("unknown message ~p~n", [Unknown]),
            loop(S)
    end.

init() ->
    loop(#state{events=dict:new(), clients=dict:new()}).

start() ->
    register(?MODULE, Pid=spawn(?MODULE, init, [])),
    Pid.

start_link() ->
    register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
    Pid.

terminate() ->
    case whereis(?MODULE) of
        undefined -> {error, already_terminated};
        _ -> ?MODULE ! shutdown
    end.

subscribe(Client) ->
    case whereis(?MODULE) of
        undefined -> {error, already_terminated};
        Pid ->
            Ref = erlang:monitor(process, Pid),
            ?MODULE ! {self(), Ref, {subscribe, Client}},
            receive
                {Ref, ok} ->
                    {ok, Ref};
                {'DOWN', Ref, process, _Pid, _Reason} ->
                    {error, _Reason}
            after 3000 ->
                {error, timeout}
            end
    end.

add_event(Name, Desc, Timeout) ->
    case whereis(?MODULE) of
        undefined -> {error, already_terminated};
        _ ->
            Ref = make_ref(),
            ?MODULE ! {self(), Ref, {add, Name, Desc, Timeout}},
            receive
                {Ref, Result} -> Result
            after 3000 ->
                {error, timeout}
            end
    end.


hotload() ->
    case whereis(?MODULE) of
        undefined -> {error, already_terminated};
        _ -> ?MODULE ! code_change
    end.
