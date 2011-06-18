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
          client_heartbeat = undefined,
          heartbeat_interval,
          close_timeout,
          timer_ref = undefined,
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
    {ok, #state{
       session_id = SessionId,
       server_module = ServerModule,
       connection_reference = {'xhr-multipart', none},
       req = Req,
       caller = Caller,
       close_timeout = CloseTimeout,
       heartbeat_interval = HeartbeatInterval,
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
                                                         connection_reference = {'xhr-multipart', connected},
                                                         timer_ref = OldTimerRef,
                                                         event_manager = EventManager } = State) ->
    Msgs = [socketio_data:decode(#msg{content=Data}) || {"data", Data} <- ServerModule:parse_post(Req)],
    F = fun(#heartbeat{index = HeartbeatNumber}, _Acc) ->
            {timer, reset_timer(OldTimerRef, Interval, heartbeat), HeartbeatNumber};
        (M, Acc) ->
            gen_event:notify(EventManager, {message, self(), M}),
            Acc
    end,
    NewState = case lists:foldl(F, undefined, lists:flatten(Msgs)) of
        {timer, NewTimerRef, HeartbeatNumber} ->
            State#state{ timer_ref = NewTimerRef, client_heartbeat = HeartbeatNumber };
        undefined ->
            State
    end,
    ServerModule:respond(Req, 200, [{"Content-Type", "text/plain"}], "ok"),
    {reply, ok, NewState};

handle_call({'xhr-multipart', data, _Reg}, _From, #state{ connection_reference = {'xhr-multipart', none} } = State) ->
    {reply, ok, State};

%% Event management
handle_call(event_manager, _From, #state{ event_manager = EventMgr } = State) ->
    {reply, EventMgr, State};

%% Sessions
handle_call(session_id, _From, #state{ session_id = SessionId } = State) ->
    {reply, SessionId, State};

%% Initial request
handle_call(req, _From, #state{ req = Req } = State) ->
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
handle_cast({initialize, Req}, #state{ server_module = ServerModule,
                                       timer_ref = OldTimerRef,
                                       heartbeat_interval = Interval } = State) ->
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
                           timer_ref = reset_timer(OldTimerRef, Interval, heartbeat) }};

%% Send
handle_cast({send, Message}, #state{ req = Req, 
                                     server_module = ServerModule,
                                     connection_reference = {'xhr-multipart', connected},
                                     timer_ref = OldTimerRef,
                                     heartbeat_interval = Interval } = State) ->
    send_message(Message, Req, ServerModule),
    {noreply, State#state{ timer_ref = reset_timer(OldTimerRef, Interval, heartbeat) }};

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
%% A client has disconnected. We fire a timer (ConnectionTimeout)!
handle_info({'EXIT', _Port, _Reason}, #state{ close_timeout = ConnectionTimeout,
                                            timer_ref = OldTimerRef } = State) when is_port(_Port) ->
    NewTimer = reset_timer(OldTimerRef, ConnectionTimeout, connection_timeout),
    {noreply, State#state { connection_reference = {'xhr-multipart', none}, timer_ref = NewTimer}};


%% General information about timer handling:
%%     There are two timer types in this transport.
%%     The heartbeat and connection_timeout. We can't use gen_server timeouts,
%%     because an unrelated incoming message would stop the timeout and a mixed
%%     solution (timer for heartbeat, gen_server timeout for disconnect) would
%%     result in the heartbeat timer killing the gen_server timeout.
%%     
%%     The connection_reference must be checked for all, because it could happen
%%     that the timer fires at exactly the time we tried to reset it (during
%%     handle_info({'EXIT', ...) for example).

handle_info({timeout, _OldTimerRef, connection_timeout}, #state{ timer_ref = _OldTimerRef,
                                                                 connection_reference = {'xhr-multipart', none} } = State) ->
    shutdown(State),
    {stop, shutdown, State};

handle_info({timeout, _OldTimerRef, heartbeat}, #state{ heartbeats = Beats,
                                                        connection_reference = {'xhr-multipart', connected},
                                                        client_heartbeat = ClientHeartbeat,
                                                        timer_ref = _OldTimerRef } = State) ->
    Client = case {ClientHeartbeat, Beats} of
        {undefined, 0} -> 0;
        _Any -> ClientHeartbeat
    end,
    case Client of
        Beats ->
            Beats1 = Beats + 1,
            socketio_client:send(self(), #heartbeat{ index = Beats1 }),
            {noreply, State#state { heartbeats = Beats1 }};
        _Other ->
            shutdown(State),
            {stop, shutdown, State}
    end;

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

reset_timer(TimerRef, Time, Message) ->
    catch(erlang:cancel_timer(TimerRef)),
    erlang:start_timer(Time, self(), Message).

shutdown(#state{ server_module = ServerModule,
                 req = Req,
                 caller = Caller }) ->
    gen_server:reply(Caller, ServerModule:respond(Req, 200, "")),
    ok.
