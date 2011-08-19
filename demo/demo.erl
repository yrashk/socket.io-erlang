#! /usr/bin/env escript
%%! -pa ../ebin ../deps/misultin/ebin ../deps/ossp_uuid/ebin ../deps/jsx/ebin 
-mode(compile).
-include_lib("../include/socketio.hrl").
-compile(export_all).
-behaviour(gen_event).

main(_) ->
    appmon:start(),
    application:start(sasl),
    application:start(misultin),
    application:start(socketio),
    {ok, Pid} = socketio_listener:start([{http_port, 7878}, 
                                         {default_http_handler,?MODULE}]),
    {ok, Pid} = socketio_listener:start([{http_port, 7878}, 
                                         {default_http_handler,?MODULE}]),
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
    Struct = [[{<<"action">>,<<"feedback">>},
                {<<"payload">>,
                [{<<"context">>,<<"order_ack">>},
                 {<<"report_id">>,90},
                 {<<"report_type">>,<<"new">>},
                 {<<"order_id">>,76},
                 {<<"user_id">>,<<"stig">>},
                 {<<"client_order_id">>,6},
                 {<<"client_head_id">>,6},
                 {<<"order_type">>,<<"new">>},
                 {<<"status">>,<<"new">>},
                 {<<"fill_type">>,<<"day">>},
                 {<<"book_id">>,<<"usd_rate_outright_2">>},
                 {<<"side">>,<<"bid">>},
                 {<<"price">>,9000},
                 {<<"order_qty">>,120},
                 {<<"head_qty">>,120},
                 {<<"display_qty">>,<<"undefined">>},
                 {<<"fill_qty">>,0},
                 {<<"left_qty">>,120},
                 {<<"child_id">>,<<"undefined">>},
                 {<<"head_id">>,76}]}]],
    socketio_client:send(Client, #msg{ content = "hello!" }),
    socketio_client:send(Client, #msg{ content = Struct, json = true}),
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

handle_request('GET', [], Req) ->
    Req:file(filename:join([filename:dirname(code:which(?MODULE)), "index.html"]));
handle_request(_Method, _Path, Req) ->
    Req:respond(200).
