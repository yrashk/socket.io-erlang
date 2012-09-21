#! /usr/bin/env escript
%%! -pa ../ebin ../deps/misultin/ebin ../deps/jsx/ebin 
-mode(compile).
-include_lib("../include/socketio.hrl").
-compile(export_all).
-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
          terminate/2, code_change/3]).
          
main(_) ->
    appmon:start(),
    application:start(sasl),
    application:start(misultin),
    application:start(socketio),
    {ok, Pid} = socketio_listener:start([{http_port, 7878}, 
                                         {default_http_handler,?MODULE},
					 {ssl, [
						{certfile, "test_certificate.pem"},
						{keyfile, "test_privkey.pem"},
						{password, "misultin"}
					       ]}]),
    EventMgr = socketio_listener:event_manager(Pid),
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

handle_event(_E, State) ->
    {ok, State}.

handle_call(_, State) ->
    {reply, ok, State}.

handle_info(_, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%

handle_request('GET', [], Req) ->
    Req:file(filename:join([filename:dirname(code:which(?MODULE)), "index_ssl.html"]));
handle_request(_Method, _Path, Req) ->
    Req:respond(200).
