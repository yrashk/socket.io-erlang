-module(socketio_transport_htmlfile).
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
init([Sup, SessionId, ServerModule, {'htmlfile', {Req, Caller}}]) ->
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
       connection_reference = {'htmlfile', none},
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
handle_call({'htmlfile', data, Req}, _From, #state{ heartbeat_interval = Interval,
                                                    timer_ref = OldTimerRef,
                                                    connection_reference = {'htmlfile', connected},
                                                    server_module = ServerModule,
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

handle_call({'htmlfile', data, _Req}, _From, #state{ connection_reference = {'htmlfile', none} } = State) ->
    {reply, ok, State};

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
handle_cast({initialize, Req}, #state{ heartbeat_interval = Interval,
                                       timer_ref = OldTimerRef,
                                       server_module = ServerModule } = State) ->
    ServerModule:headers(Req, [{"Content-Type", "text/html"},
                               {"Connection", "Keep-Alive"},
                               {"Transfer-Encoding", "chunked"}]),
    H = "<html><body>" ++ lists:duplicate(254, $\s),
    link(ServerModule:socket(Req)),
    ServerModule:chunk(Req, H),
    {noreply, State#state{ connection_reference = {htmlfile, connected},
                           timer_ref = reset_timer(OldTimerRef, Interval, heartbeat) }};

%% Send
handle_cast({send, Message}, #state{ req = Req, 
                                     server_module = ServerModule,
                                     connection_reference = {'htmlfile', connected},
                                     timer_ref = OldTimerRef,
                                     heartbeat_interval = Interval } = State) ->
    send_message(Message, ServerModule, Req),
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
%% CLient disconnected. We fire a timer (ServerTimeout)!
handle_info({'EXIT',_Port,_Reason}, #state{ close_timeout = ServerTimeout,
                                            timer_ref = OldTimerRef } = State) when is_port(_Port) ->
    NewTimer = reset_timer(OldTimerRef, ServerTimeout, connection_timeout),
    {noreply, State#state { connection_reference = {'htmlfile', none}, timer_ref = NewTimer }};

handle_info({timeout, _Ref, connection_reference}, #state{ timer_ref = _Ref,
                                                           connection_reference = {'htmlfile', none} } = State) ->
    shutdown(State),
    {stop, shutdown, State};

%% Regular heartbeat
handle_info({timeout, _Ref, heartbeat}, #state{ heartbeats = Beats,
                                                connection_reference = {'htmlfile', connected},
                                                timer_ref = _Ref,
                                                client_heartbeat = ClientHeartbeat } = State) ->
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
send_message(#msg{} = Message, ServerModule, Req) ->
    send_message(socketio_data:encode(Message), ServerModule,Req);

send_message(#heartbeat{} = Message, ServerModule, Req) ->
    send_message(socketio_data:encode(Message), ServerModule, Req);

send_message(Message, ServerModule, Req) ->
    Message0 =  binary_to_list(jsx:term_to_json(list_to_binary(Message), [{strict, false}])),
    M = "<script>parent.s._(" ++ Message0 ++ ", document);</script>",
    apply(ServerModule, chunk, [Req, M]).

reset_timer(TimerRef, Time, Message) ->
    catch(erlang:cancel_timer(TimerRef)),
    erlang:start_timer(Time, self(), Message).

shutdown(#state{ server_module = ServerModule,
                 req = Req,
                 caller = Caller }) ->
    gen_server:reply(Caller, ServerModule:respond(Req, 200, "")),
    ok.