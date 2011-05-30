-module(socketio_http).
-include_lib("../include/socketio.hrl").
-behaviour(gen_server).

%% API
-export([start_link/6]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
          default_http_handler,
          sessions,
          event_manager,
          sup,
          web_server_monitor,
          server_module,
          resource,
          last_generated_pid
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
start_link(ServerModule, Port, Resource, SSL, {DefaultHttpHandler, Args}, Sup) ->
    gen_server:start_link(?MODULE, [ServerModule, Port, Resource, SSL, {DefaultHttpHandler, Args}, Sup], []);
start_link(ServerModule, Port, Resource, SSL, DefaultHttpHandler, Sup) ->
    gen_server:start_link(?MODULE, [ServerModule, Port, Resource, SSL, DefaultHttpHandler, Sup], []).

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
init([ServerModule, Port, Resource, SSL, DefaultHttpHandler, Sup]) ->
    Self = self(),
    process_flag(trap_exit, true),
    {ok, ServerPid} = apply(ServerModule, start_link, [[{port, Port}, {http_process, Self}, {resource, Resource}, {ssl, SSL}]]),
    WebServerRef = erlang:monitor(process, ServerPid),
    gen_server:cast(Self, acquire_event_manager),
    {ok, #state{
       default_http_handler = DefaultHttpHandler,
       sessions = ets:new(socketio_sessions,[public]),
       sup = Sup,
       web_server_monitor = WebServerRef,
       server_module = ServerModule,
       resource = Resource,
       last_generated_pid = undefined
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
handle_call({request, 'GET', ["socket.io.js"|Resource], Req}, _From, #state{ server_module = ServerModule, 
                                                                             resource = Resource } = State) ->
    Response = apply(ServerModule, file, [Req, filename:join([filename:dirname(code:which(?MODULE)), "..", "priv", "Socket.IO", "socket.io.js"])]),
    {reply, Response, State};

handle_call({request, 'GET', ["WebSocketMain.swf", "web-socket-js", "vendor", "lib"|Resource], Req}, _From,
            #state{ server_module = ServerModule, resource = Resource } = State) ->
    Response = apply(ServerModule, file, [Req, filename:join([filename:dirname(code:which(?MODULE)), "..", "priv", "Socket.IO", "lib", "vendor", "web-socket-js", "WebSocketMain.swf"])]),
    {reply, Response, State};

%% New XHR Polling request
handle_call({request, 'GET', [_Random, "xhr-polling"|Resource], Req }, From, #state{ resource = Resource} = State) ->
    handle_call({session, generate, {'xhr-polling', Req}, socketio_transport_polling}, From, State);

%% Returning XHR Polling
handle_call({request, 'GET', [_Random, SessionId, "xhr-polling"|Resource], Req }, From, 
            #state{ server_module = ServerModule,
                    resource = Resource, sessions = Sessions } = State) ->
    case ets:lookup(Sessions, SessionId) of
        [{SessionId, Pid}] -> 
            gen_server:cast(Pid, {'xhr-polling', polling_request, Req, From});
        _ ->
            gen_server:reply(From, apply(ServerModule, respond, [Req, 404, ""]))
    end,
    {noreply, State};

%% Incoming XHR Polling data
handle_call({request, 'POST', ["send", SessionId, "xhr-polling"|Resource], Req }, _From, #state{ resource = Resource, 
                                                                                                 server_module = ServerModule,
                                                                                                 sessions = Sessions } = State) ->
    Response =  
        case ets:lookup(Sessions, SessionId) of
            [{SessionId, Pid}] -> 
                gen_server:call(Pid, {'xhr-polling', data, Req});
            _ ->
                apply(ServerModule, respond, [Req, 404, ""])
        end,
    {reply, Response, State};

%% New JSONP Polling request
handle_call({request, 'GET', [Index, _Random, "jsonp-polling"|Resource], Req }, From, #state{ resource = Resource} = State) ->
    handle_call({session, generate, {'jsonp-polling', {Req, Index}}, socketio_transport_polling}, From, State);

%% Returning JSONP Polling
handle_call({request, 'GET', [Index, _Random, SessionId, "jsonp-polling"|Resource], Req }, From, 
            #state{ resource = Resource, 
                    server_module = ServerModule,
                    sessions = Sessions } = State) ->
    case ets:lookup(Sessions, SessionId) of
        [{SessionId, Pid}] ->
            gen_server:cast(Pid, {'jsonp-polling', polling_request, {Req, Index}, From});
        _ ->
            gen_server:reply(From, apply(ServerModule, respond, [Req, 404, ""]))
    end,
    {noreply, State};

%% Incoming JSONP Polling data
handle_call({request, 'POST', [_Index, _Random, SessionId, "jsonp-polling"|Resource], Req }, _From, 
            #state{ resource = Resource, 
                    server_module = ServerModule,
                    sessions = Sessions } = State) ->
    Response =  
        case ets:lookup(Sessions, SessionId) of
            [{SessionId, Pid}] -> 
                gen_server:call(Pid, {'jsonp-polling', data, Req});
            _ ->
                apply(ServerModule, respond, [Req, 404, ""])
        end,
    {reply, Response, State};

%% New XHR Multipart request
handle_call({request, 'GET', ["xhr-multipart"|Resource], Req }, From, #state{ resource = Resource} = State) ->
    {reply, _Reply, NewState} = handle_call({session, generate, {'xhr-multipart', {Req, From}}, socketio_transport_xhr_multipart}, From, State),
    {noreply, NewState};
    

%% Incoming XHR Multipart data
handle_call({request, 'POST', ["send", SessionId, "xhr-multipart"|Resource], Req }, _From, 
            #state{ resource = Resource, 
                    server_module = ServerModule,
                    sessions = Sessions } = State) ->
    Response =  
        case ets:lookup(Sessions, SessionId) of
            [{SessionId, Pid}] -> 
                gen_server:call(Pid, {'xhr-multipart', data, Req});
            _ ->
                apply(ServerModule, respond, [Req, 404, ""])
        end,
    {reply, Response, State};


%% New htmlfile request
handle_call({request, 'GET', [_Random, "htmlfile"|Resource], Req }, From, #state{ resource = Resource} = State) ->
    {reply, _Reply, NewState} = handle_call({session, generate, {'htmlfile', {Req, From}}, socketio_transport_htmlfile}, From, State),
    {noreply, NewState};

%% Incoming htmlfile data
handle_call({request, 'POST', ["send", SessionId, "htmlfile"|Resource], Req }, _From, 
            #state{ resource = Resource, 
                    server_module = ServerModule,
                    sessions = Sessions } = State) ->
    Response =  
        case ets:lookup(Sessions, SessionId) of
            [{SessionId, Pid}] ->
                gen_server:call(Pid, {'htmlfile', data, Req});
            _ ->
                apply(ServerModule, respond, [Req, 404, ""])
        end,
    {reply, Response, State};


%% If we can't route it, let others deal with it
handle_call({request, _Method, _Path, _Req} = Req, From, #state{ default_http_handler = {HttpHandler,Args} } = State) when is_atom(HttpHandler) ->
    handle_call(Req, From, State#state{ default_http_handler = fun(P1, P2, P3) -> apply(HttpHandler, handle_request, [P1, P2, P3 | Args]) end });
handle_call({request, _Method, _Path, _Req} = Req, From, #state{ default_http_handler = HttpHandler } = State) when is_atom(HttpHandler) ->
    handle_call(Req, From, State#state{ default_http_handler = fun(P1, P2, P3) -> HttpHandler:handle_request(P1, P2, P3) end });

handle_call({request, Method, Path, Req}, _From, #state{ default_http_handler = HttpHandler } = State) when is_function(HttpHandler) ->
    Response = HttpHandler(Method, lists:reverse(Path), Req),
    {reply, Response, State};

%% Sessions
handle_call({session, generate, ConnectionReference, Transport}, _From, #state{ 
                                                                   sup = Sup,
                                                                   sessions = Sessions,
                                                                   event_manager = EventManager,
                                                                   server_module = ServerModule
                                                                  } = State) ->
    UUID = binary_to_list(ossp_uuid:make(v4, text)),
    {ok, Pid} = socketio_client:start(Sup, Transport, UUID, ServerModule, ConnectionReference),
    link(Pid),
    ets:insert(Sessions, [{UUID, Pid}, {Pid, UUID}]),
    gen_event:notify(EventManager, {client, Pid}),
    {reply, {UUID, Pid}, State#state{last_generated_pid = Pid}};

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

handle_cast({closed, Data}, #state{ sessions = Sessions, last_generated_pid = LastPid } = State) ->
    case has_transport_process(Data, Sessions, LastPid) of
        [{Pid, _SessionID}] when Pid /= undefined ->
            catch(gen_server:call(Pid, stop));
        _Any -> ok
    end,
    {noreply, State};

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

% If misultin goes down, we stop socket.io
handle_info({'DOWN', WebServerRef, _, _, _}, #state{ web_server_monitor = WebServerRef } = State) ->
    {stop, normal, State};             

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
listener(#state{ sup = Sup }) ->
    socketio_listener:server(Sup).

has_transport_process([_Random, SessionId, "xhr-polling"|_Resource], Sessions, _LastGeneratedPid) -> ets:lookup(Sessions, SessionId);
has_transport_process([_Random, "xhr-polling"|_Resource], _Sessions, LastGeneratedPid) -> [{LastGeneratedPid, ok}];

has_transport_process([_Index, _Random, SessionId, "jsonp-polling"|_Resource], Sessions, _LastGeneratedPid) -> ets:lookup(Sessions, SessionId);
has_transport_process([_Random, "xhr-polling"|_Resource], _Sessions, LastGeneratedPid) -> [{LastGeneratedPid, ok}];

has_transport_process(["send", SessionId, "xhr-multipart"|_Resource], Sessions, _LastGeneratedPid) -> ets:lookup(Sessions, SessionId);
has_transport_process(["xhr-multipart"|_Resource], _Sessions, LastGeneratedPid) -> [{LastGeneratedPid, ok}];

has_transport_process(["send", SessionId, "htmlfile"|_Resource], Sessions, _LastGeneratedPid) -> ets:lookup(Sessions, SessionId);
has_transport_process([_Random, "htmlfile"|_Resource], _Sessions, LastGeneratedPid) -> [{LastGeneratedPid, ok}];

has_transport_process(_, _Sessions, _LastGeneratedPid) -> [].