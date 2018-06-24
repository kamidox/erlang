-module(rmd_event).
-export([start/2, start_link/2, cancel/1, init/3]).
-record(state, {
          server,
          name="",
          togo=0
         }).

loop(State) ->
    #state{server=Server, name=Name, togo=ToGo} = State,
    receive
        {Server, Ref, cancel} -> Server ! {Ref, ok}
    after ToGo * 1000 ->
        Server ! {done, Name}
    end.

init(Server, Name, Delay) ->
    loop(#state{server=Server, name=Name, togo=Delay}).

start(Name, Delay) ->
    spawn(?MODULE, init, [self(), Name, Delay]).

start_link(Name, Delay) ->
    spawn_link(?MODULE, init, [self(), Name, Delay]).

cancel(EventPid) ->
    Ref = erlang:monitor(process, EventPid),
    EventPid ! {self(), Ref, cancel},
    receive
        {Ref, ok} ->
            erlang:demonitor(Ref, [flush]),
            ok;
        {'DOWN', Ref, process, EventPid, _Reason} ->
            ok
    end.

