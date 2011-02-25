-module(socketio_http).
-include_lib("socketio.hrl").
-behaviour(gen_server).

%% API
-export([start_link/3, start/3]).
%% Internal API
-export([heartbeat/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
          default_http_handler,
          message_handler,
          sessions
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
start_link(Port, DefaultHttpHandler, MessageHandler) ->
    gen_server:start_link(?MODULE, [Port, DefaultHttpHandler, MessageHandler], []).

start(Port, DefaultHttpHandler, MessageHandler) ->
    supervisor:start_child(socketio_http_sup, [Port, DefaultHttpHandler, MessageHandler]).

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
init([Port, DefaultHttpHandler, MessageHandler]) ->
    Self = self(),
    misultin:start_link([{port, Port},
                         {loop, fun (Req) -> handle_http(Self, Req) end},
                         {ws_loop, fun (Ws) -> handle_websocket(Self, Ws) end},
                         {ws_autoexit, false}
                        ]),
    {ok, #state{
       default_http_handler = DefaultHttpHandler,
       message_handler = MessageHandler,
       sessions = ets:new(socketio_sessions,[public])
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
handle_call({request, {abs_path, "/socket.io.js"}, Req}, _From, State) ->
    Response = Req:file(filename:join([filename:dirname(code:which(?MODULE)), "..", "priv", "Socket.IO", "socket.io.js"])),
    {reply, Response, State};

%% If we can't route it, let others deal with it
handle_call({request, _, _} = Req, From, #state{ default_http_handler = HttpHandler } = State) when is_atom(HttpHandler) ->
    handle_call(Req, From, State#state{ default_http_handler = fun(P1, P2) -> HttpHandler:handle_request(P1, P2) end });

handle_call({request, Path, Req}, _From, #state{ default_http_handler = HttpHandler } = State) when is_function(HttpHandler) ->
    Response = HttpHandler(Path, Req),
    {reply, Response, State};

%% Websockets
handle_call({websocket, _Data, _SessionID, _Ws} = Req, From, #state{ message_handler = Handler} = State) when is_atom(Handler)  ->
    handle_call(Req, From, State#state{ message_handler = fun(P1, P2, P3) -> Handler:handle_message(P1, P2, P3) end });

handle_call({websocket, Data, SessionID, _Ws}, _From, #state{ message_handler = Handler} = State) when is_function(Handler)  ->
    Self = self(),
    spawn_link(fun () ->
                       socketio_data:parse(fun (Parser) -> socketio_data:string_reader(Parser, Data) end,
                                           fun (#heartbeat{}) ->
                                                   ignore; %% FIXME: we should actually reply
                                               (M) -> Handler(Self, SessionID, M) end)
               end),
    {reply, ok, State};

handle_call({websocket, _}, _From, State) ->
    {reply, ok, State};

%% Sessions
handle_call({session, generate, ConnectionReference}, _From, #state{ sessions = Sessions } = State) ->
    UUID = binary_to_list(ossp_uuid:make(v4, text)),
    ets:insert(Sessions, {UUID, ConnectionReference}),
    case application:get_env(heartbeat_interval) of
        {ok, Time} ->
            timer:apply_after(Time, ?MODULE, heartbeat, [ConnectionReference, 0]);
        _ ->
            ignore
    end,
    {reply, UUID, State};

handle_call({session, expire, SessionID}, _From, #state{ sessions = Sessions } = State) ->
    ets:delete(Sessions, SessionID),
    {reply, ok, State};

%% Send
handle_call({send, SessionID, #msg{} = Message}, _From, #state{ sessions = Sessions } = State) ->
    case ets:lookup(Sessions, SessionID) of
        [{SessionID, ConnectionReference}] ->
            handle_send(ConnectionReference, Message),
            {reply, ok, State};
        [] ->
            {reply, {error, invalid_session}, State}
    end.


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
    gen_server:call(Server, {request, Req:get(uri), Req}).

handle_websocket(Server, Ws) ->
    SessionID = gen_server:call(Server, {session, generate, {websocket, Ws}}),
    ok = gen_server:call(Server, {send, SessionID, #msg{ content = SessionID }}),
    handle_websocket(Server, Ws, SessionID).

handle_websocket(Server, Ws, SessionID) ->
    receive
        {browser, Data} ->
            gen_server:call(Server, {websocket, Data, SessionID, Ws}),
            handle_websocket(Server, Ws, SessionID);
        closed ->
            gen_server:call(Server, {session, expire, SessionID});
        _Ignore ->
            handle_websocket(Server, Ws, SessionID)
    end.

handle_send({websocket, Ws}, Message) ->
    Ws:send(socketio_data:encode(Message)).

heartbeat(ConnectionReference, Beats) ->
    Beats1 = Beats + 1,
    handle_send(ConnectionReference, #heartbeat{ index = Beats1 }),
    case application:get_env(heartbeat_interval) of
        {ok, Time} ->
            timer:apply_after(Time, ?MODULE, heartbeat, [ConnectionReference, Beats1]);
        _ ->
            ignore
    end.

