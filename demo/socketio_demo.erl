-module(socketio_demo).

-include("../include/socketio.hrl").

-behaviour(gen_event).

%% API
-export([start/0,
         handle_request/3]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

start() ->
    ok = start_apps([sasl, misultin, socketio]),
    {ok, Pid} = socketio_listener:start([{http_port, 7878},
                                         {default_http_handler,?MODULE}]),
    {ok, Pid} = socketio_listener:start([{http_port, 7878}, 
                                         {default_http_handler,?MODULE}]),
    {ok, Pid} = socketio_listener:start([{http_port, 7878}, 
                                         {default_http_handler,?MODULE}]),
    EventMgr = socketio_listener:event_manager(Pid),
    ok = gen_event:add_handler(EventMgr, ?MODULE,[]).

init([]) ->
    {ok, #state{}}.

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
handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_request('GET', [], Req) ->
    Req:file(filename:join([filename:dirname(code:which(?MODULE)), "index.html"]));
handle_request(_Method, _Path, Req) ->
    Req:respond(200).

%% Internal functions
start_apps([]) ->
    ok;
start_apps([App|T]) ->
    case application:start(App) of
        ok ->
            start_apps(T);
        {error, already_started} ->
            start_apps(T);
        Error ->
            Error
    end.
