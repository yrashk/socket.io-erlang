-module(socketio_transport_xhr_multipart).
-include_lib("../include/socketio.hrl").
-behaviour(gen_server).

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
          session_id,
          req,
          caller,
          server_module,
          connection_reference,
          heartbeats = 0,
          heartbeat_interval,
          close_timeout,
          event_manager,
          sup
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
start_link(Sup, SessionId, ServerModule, ConnectionReference) ->
    gen_server:start_link(?MODULE, [Sup, SessionId, ServerModule, ConnectionReference], []).

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
init([Sup, SessionId, ServerModule, {'xhr-multipart', {Req, Caller}}]) ->
    apply(ServerModule, ensure_longpolling_request, [Req]),
    process_flag(trap_exit, true),
    HeartbeatInterval = 
    case application:get_env(heartbeat_interval) of
        {ok, Time} ->
            Time;
        _ ->
            error_logger:warning_report(
                "Could not load default heartbeat_interval value from "
                "the application file. Setting the default value to 10000."
            ),
            10000
    end,
    CloseTimeout = 
    case application:get_env(close_timeout) of
        {ok, Time0} ->
            Time0;
        _ ->
            error_logger:warning_report(
                "Could not load default close_timeout value from "
                "the application file. Setting the default value to 8000 ms."
            ),
            8000
    end,
    {ok, EventMgr} = gen_event:start_link(),
    gen_server:cast(self(), {initialize, Req}),
    socketio_client:send(self(), #msg{ content = SessionId }),
    gen_server:cast(self(), heartbeat),
    {ok, #state{
       session_id = SessionId,
       server_module = ServerModule,
       connection_reference = {'xhr-multipart', none},
       req = Req,
       caller = Caller,
       close_timeout = CloseTimeout,
       heartbeat_interval = {make_ref(), HeartbeatInterval},
       event_manager = EventMgr,
       sup = Sup
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
%% Incoming data
handle_call({'xhr-multipart', data, Req}, _From, #state{ server_module = ServerModule,
                                                         heartbeat_interval = Interval,
                                                         event_manager = EventManager } = State) ->
    Msgs = [socketio_data:decode(#msg{content=Data}) || {"data", Data} <- ServerModule:parse_post(Req)],
    F = fun(#heartbeat{}, _Acc) ->
            {timer, reset_heartbeat(Interval)};
        (M, Acc) ->
            gen_event:notify(EventManager, {message, self(), M}),
            Acc
    end,
    NewState = case lists:foldl(F, undefined, lists:flatten(Msgs)) of
        {timer, NewInterval} ->
            State#state{ heartbeat_interval = NewInterval};
        undefined ->
            State
    end,
    ServerModule:respond(Req, 200, [{"Content-Type", "text/plain"}], "ok"),
    {reply, ok, NewState};

%% Event management
handle_call(event_manager, _From, #state{ event_manager = EventMgr } = State) ->
    {reply, EventMgr, State};

%% Sessions
handle_call(session_id, _From, #state{ session_id = SessionId } = State) ->
    {reply, SessionId, State};

%% Initial request
handle_call(req, _From, #state{ req = Req} = State) ->
    {reply, Req, State};

%% Flow control
handle_call(stop, _From, State) ->
    {stop, shutdown, State}.


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
handle_cast({initialize, Req}, #state{ server_module = ServerModule, heartbeat_interval = Interval } = State) ->
    Headers = ServerModule:get_headers(Req),
    Headers1 =
    case proplists:get_value('Origin', Headers) of
        undefined ->
            Headers;
        Origin ->
            case socketio_listener:verify_origin(Origin, socketio_listener:origins(listener(State))) of
                true ->
                    [{"Access-Control-Allow-Origin", "*"},
                     {"Access-Control-Allow-Credentials", "true"} | Headers];
                false ->
                    Headers
            end
    end,
    link(ServerModule:socket(Req)),
    ServerModule:headers(Req, [{"Content-Type", "multipart/x-mixed-replace;boundary=\"socketio\""},
                               {"Connection", "Keep-Alive"} | Headers1]),
    ServerModule:stream(Req, "--socketio\n"),
    {noreply, State#state{ connection_reference = {'xhr-multipart', connected},
                           heartbeat_interval = reset_heartbeat(Interval) }};

handle_cast(heartbeat, #state{ heartbeats = Beats,
                               heartbeat_interval = Interval } = State) ->
    Beats1 = Beats + 1,
    socketio_client:send(self(), #heartbeat{ index = Beats1 }),
    {noreply, State#state { heartbeats = Beats1,
                            heartbeat_interval = reset_heartbeat(Interval) }};

%% Send
handle_cast({send, Message}, #state{ req = Req, 
                                     server_module = ServerModule,
                                     connection_reference = {'xhr-multipart', connected },
                                     heartbeat_interval = Interval } = State) ->
    send_message(Message, Req, ServerModule),
    {noreply, State#state{ heartbeat_interval = reset_heartbeat(Interval) }};

handle_cast(_, #state{} = State) ->
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
%% A client has disconnected. We fire a timer (ServerTimeout)!
handle_info({'EXIT',Connection,_Reason}, #state{ close_timeout = ServerTimeout} = State) when is_port(Connection);
											      is_pid(Connection)->
    {noreply, State#state { connection_reference = {'xhr-multipart', none}}, ServerTimeout};

%% This branch handles two purposes: 1. handling the close_timeout,
%% 2. handling the heartbeat timeout that might comes first. The thing is,
%% when the client connection dies, we need to wait for the close_timeout
%% to be fired. That one can be cancelled at any time by God knows what for now,
%% and that might be desirable. However, it can also be cancelled because we
%% happen to receive the heartbeat timeout. Given the right setting, this will
%% happen every time a disconnection happens at the point where the delay left to
%% the current heartbeat is longer than the delay left to the total value of
%% close_timout. Funny, eh?
%% For this reason, the heartbeat timeout when we have no xhr-multipart
%% connection reference has to be seen as the same as the close_timeout
%% timer firing off. This is the safest way to guarantee everything will run
%% okay.
handle_info(timeout, #state{ server_module = ServerModule,
                             connection_reference = {'xhr-multipart', none}, req = Req, caller = Caller } = State) ->
    gen_server:reply(Caller, ServerModule:respond(Req, 200, "")),
    {stop, shutdown, State};

%% See the previous clauses' comments, please.
handle_info({timeout, _Ref, heartbeat}, #state{ connection_reference = {'xhr-multipart', none} } = State) ->
    handle_info(timeout, State);

%% Good old regular heartbeat. I admire your simplicity.
handle_info({timeout, _Ref, heartbeat}, State) ->
    gen_server:cast(self(), heartbeat),
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
send_message(#msg{} = Message, Req, ServerModule) ->
    send_message(socketio_data:encode(Message), Req, ServerModule);

send_message(#heartbeat{} = Message, Req, ServerModule) ->
    send_message(socketio_data:encode(Message), Req, ServerModule);

send_message([6] = Message, Req, ServerModule) ->
    apply(ServerModule, stream, [Req,"Content-Type: text/plain; charset=us-ascii\n\n" ++ Message ++ "\n--socketio\n"]);
send_message(Message, Req, ServerModule) ->
    apply(ServerModule, stream, [Req,"Content-Type: text/plain\n\n" ++ Message ++ "\n--socketio\n"]).

listener(#state{ sup = Sup }) ->
    socketio_listener:server(Sup).

reset_heartbeat({TimerRef, Time}) ->
    erlang:cancel_timer(TimerRef),
    NewRef = erlang:start_timer(Time, self(), heartbeat),
    {NewRef, Time}.
