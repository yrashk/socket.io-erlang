-module(socketio_listener).
-export([start/1]).

start(Options) ->
    {ok, Pid} = supervisor:start_child(socketio_listener_sup_sup, [Options]),
    {dictionary, Dict} = erlang:process_info(Pid, dictionary),
    EventMgr = proplists:get_value(event_manager, Dict),
    {ok, Pid, EventMgr}.
