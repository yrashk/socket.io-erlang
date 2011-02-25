#! /usr/bin/env escript
%%! -pa ../ebin ../deps/misultin/ebin ../deps/ossp_uuid/ebin ../deps/jsx/ebin 
-mode(compile).
-include_lib("../include/socketio.hrl").
-compile(export_all).
-behaviour(gen_event).

main(_) ->
    application:start(sasl),
    application:start(socketio),
    {ok, Pid} = socketio_http:start(7878, ?MODULE),
    EventMgr = socketio_http:event_manager(Pid),
    ok = gen_event:add_handler(EventMgr, ?MODULE,[]),
    receive _ -> ok end.

%% gen_event
init([]) ->
    {ok, undefined}.

handle_event({client, Pid}, State) ->
    io:format("Connected: ~p~n",[Pid]),
    EventMgr = socketio_client:event_manager(Pid),
    ok = gen_event:add_handler(EventMgr, ?MODULE,[]),
    {ok, State};
handle_event({disconnect, Pid}, State) ->
    io:format("Disconnected: ~p~n",[Pid]),
    {ok, State};
handle_event({message, Client, #msg{ content = Content } = Msg}, State) ->
    io:format("Got a message: ~p from ~p~n",[Msg, Client]),
    socketio_client:send(Client, #msg{ content = "hello!" }),
    socketio_client:send(Client, #msg{ content = [{<<"echo">>, Content}], json = true}),
    {ok, State};

handle_event(E, State) ->
    {ok, State}.

handle_call(_, State) ->
    {reply, ok, State}.

handle_info(_, State) ->
    {ok, State}.

terminate(_Reason, State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%

handle_request({abs_path, "/"}, Req) ->
    Req:file(filename:join([filename:dirname(code:which(?MODULE)), "index.html"]));
handle_request({abs_path, _Path}, Req) ->
    Req:respond(200).
