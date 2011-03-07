-module(socketio_http).
-include_lib("../include/socketio.hrl").
-behaviour(gen_server).

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
          default_http_handler,
          sessions,
          event_manager,
          sup,
          resource
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Port, Resource, DefaultHttpHandler, Sup) ->
    gen_server:start_link(?MODULE, [Port, Resource, DefaultHttpHandler, Sup], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Port, Resource, DefaultHttpHandler, Sup]) ->
    Self = self(),
    process_flag(trap_exit, true),
    misultin:start_link([{port, Port},
                         {loop, fun (Req) -> handle_http(Self, Req) end},
                         {ws_loop, fun (Ws) -> handle_websocket(Self, Resource, Ws) end},
                         {ws_autoexit, false}
                        ]),
    gen_server:cast(Self, acquire_event_manager),
    {ok, #state{
       default_http_handler = DefaultHttpHandler,
       sessions = ets:new(socketio_sessions,[public]),
       sup = Sup,
       resource = Resource
      }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({request, 'GET', ["socket.io.js"|Resource], Req}, _From, #state{ resource = Resource } = State) ->
    Response = Req:file(filename:join([filename:dirname(code:which(?MODULE)), "..", "priv", "Socket.IO", "socket.io.js"])),
    {reply, Response, State};

handle_call({request, 'GET', ["WebSocketMain.swf", "web-socket-js", "vendor", "lib"|Resource], Req}, _From,
            #state{ resource = Resource } = State) ->
    Response = Req:file(filename:join([filename:dirname(code:which(?MODULE)), "..", "priv", "Socket.IO", "lib", "vendor", "web-socket-js", "WebSocketMain.swf"])),
    {reply, Response, State};

%% New XHR Polling request
handle_call({request, 'GET', [_Random, "xhr-polling"|Resource], Req }, From, #state{ resource = Resource} = State) ->
    handle_call({session, generate, {'xhr-polling', Req}, socketio_transport_xhr_polling}, From, State);

%% Returning XHR Polling
handle_call({request, 'GET', [_Random, SessionId, "xhr-polling"|Resource], Req }, From, #state{ resource = Resource, sessions = Sessions } = State) ->
    case ets:lookup(Sessions, SessionId) of
        [{SessionId, Pid}] -> 
            gen_server:cast(Pid, {'xhr-polling', polling_request, Req, From});
        _ ->
            gen_server:reply(From, Req:respond(404, ""))
    end,
    {noreply, State};

%% Incoming XHR Polling data
handle_call({request, 'POST', ["send", SessionId, "xhr-polling"|Resource], Req }, _From, #state{ resource = Resource, sessions = Sessions } = State) ->
    Response =  
        case ets:lookup(Sessions, SessionId) of
            [{SessionId, Pid}] -> 
                gen_server:call(Pid, {'xhr-polling', data, Req});
            _ ->
                Req:respond(404, "")
        end,
    {reply, Response, State};

%% New JSONP Polling request
handle_call({request, 'GET', [Index, _Random, "jsonp-polling"|Resource], Req }, From, #state{ resource = Resource} = State) ->
    handle_call({session, generate, {'jsonp-polling', {Req, Index}}, socketio_transport_jsonp_polling}, From, State);

%% Returning JSONP Polling
handle_call({request, 'GET', [Index, _Random, SessionId, "jsonp-polling"|Resource], Req }, From, #state{ resource = Resource, sessions = Sessions } = State) ->
    case ets:lookup(Sessions, SessionId) of
        [{SessionId, Pid}] ->
            gen_server:cast(Pid, {'jsonp-polling', polling_request, {Req, Index}, From});
        _ ->
            gen_server:reply(From, Req:respond(404, ""))
    end,
    {noreply, State};

%% Incoming JSONP Polling data
handle_call({request, 'POST', [Index, _Random, SessionId, "jsonp-polling"|Resource], Req }, _From, #state{ resource = Resource, sessions = Sessions } = State) ->
    Response =  
        case ets:lookup(Sessions, SessionId) of
            [{SessionId, Pid}] -> 
                gen_server:call(Pid, {'jsonp-polling', data, {Req, Index}});
            _ ->
                Req:respond(404, "")
        end,
    {reply, Response, State};

%% New XHR Multipart request
handle_call({request, 'GET', ["xhr-multipart"|Resource], Req }, _From, #state{ resource = Resource} = State) ->
    handle_call({session, generate, {'xhr-multipart', Req}, socketio_transport_xhr_multipart}, _From, State),
    {noreply, State};

%% Incoming XHR Multipart data
handle_call({request, 'POST', ["send", SessionId, "xhr-multipart"|Resource], Req }, _From, #state{ resource = Resource, sessions = Sessions } = State) ->
    Response =  
        case ets:lookup(Sessions, SessionId) of
            [{SessionId, Pid}] -> 
                gen_server:call(Pid, {'xhr-multipart', data, Req});
            _ ->
                Req:respond(404, "")
        end,
    {reply, Response, State};


%% New htmlfile request
handle_call({request, 'GET', [_Random, "htmlfile"|Resource], Req }, _From, #state{ resource = Resource} = State) ->
    handle_call({session, generate, {'htmlfile', Req}, socketio_transport_htmlfile}, _From, State),
    {noreply, State};

%% Incoming htmlfile data
handle_call({request, 'POST', ["send", SessionId, "htmlfile"|Resource], Req }, _From, #state{ resource = Resource, sessions = Sessions } = State) ->
    Response =  
        case ets:lookup(Sessions, SessionId) of
            [{SessionId, Pid}] -> 
                gen_server:call(Pid, {'htmlfile', data, Req});
            _ ->
                Req:respond(404, "")
        end,
    {reply, Response, State};


%% If we can't route it, let others deal with it
handle_call({request, _Method, _Path, _Req} = Req, From, #state{ default_http_handler = HttpHandler } = State) when is_atom(HttpHandler) ->
    handle_call(Req, From, State#state{ default_http_handler = fun(P1, P2, P3) -> HttpHandler:handle_request(P1, P2, P3) end });

handle_call({request, Method, Path, Req}, _From, #state{ default_http_handler = HttpHandler } = State) when is_function(HttpHandler) ->
    Response = HttpHandler(Method, lists:reverse(Path), Req),
    {reply, Response, State};

%% Sessions
handle_call({session, generate, ConnectionReference, Transport}, _From, #state{ 
                                                                   sup = Sup,
                                                                   sessions = Sessions,
                                                                   event_manager = EventManager
                                                                  } = State) ->
    UUID = binary_to_list(ossp_uuid:make(v4, text)),
    {ok, Pid} = socketio_client:start(Sup, Transport, UUID, ConnectionReference),
    link(Pid),
    ets:insert(Sessions, [{UUID, Pid}, {Pid, UUID}]),
    gen_event:notify(EventManager, {client, Pid}),
    {reply, {UUID, Pid}, State};

%% Event management
handle_call(event_manager, _From, #state{ event_manager = EventMgr } = State) ->
    {reply, EventMgr, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(acquire_event_manager, State) ->
    EventManager = socketio_listener:event_manager(listener(State)),
    {noreply, State#state{ event_manager = EventManager }};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, _}, #state{ event_manager = EventManager, sessions = Sessions } = State) ->
    case ets:lookup(Sessions, Pid) of
        [{Pid, UUID}] ->
            ets:delete(Sessions,UUID),
            ets:delete(Sessions,Pid),
            gen_event:notify(EventManager, {disconnect, Pid});
        _ ->
            ignore
    end,
    {noreply, State};
            
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_http(Server, Req) ->
    Path = Req:resource([urldecode]),
    gen_server:call(Server, {request, Req:get(method), lists:reverse(Path), Req}, infinity).

handle_websocket(Server, Resource, Ws) ->
    WsPath = Ws:get(path),
    WsResource = string:tokens(WsPath,"/"),
    handle_websocket_1(Server, Resource, lists:reverse(WsResource), Ws).

handle_websocket_1(Server, Resource, ["flashsocket"|Resource], Ws) ->
    handle_websocket_1(Server, Resource, ["websocket"|Resource], Ws);

handle_websocket_1(Server, Resource, ["websocket"|Resource], Ws) ->
    {SessionID, Pid} = gen_server:call(Server, {session, generate, {websocket, Ws}, socketio_transport_websocket}),
    handle_websocket(Server, Ws, SessionID, Pid);
handle_websocket_1(_Server, _Resource, _WsResource, _Ws) ->
    ignore. %% FIXME: pass it through to the end user?

handle_websocket(Server, Ws, SessionID, Pid) ->
    receive
        {browser, Data} ->
            gen_server:call(Pid, {websocket, Data, Ws}),
            handle_websocket(Server, Ws, SessionID, Pid);
        closed ->
            gen_server:call(Pid, stop);
        _Ignore ->
            handle_websocket(Server, Ws, SessionID, Pid)
    end.

listener(#state{ sup = Sup }) ->
    socketio_listener:server(Sup).
