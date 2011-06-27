-module(socketio_transport_polling).
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
          message_buffer = [],
          server_module,
          connection_reference,
          req,
          caller,
          index,
          polling_duration,
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
    gen_server:start_link(?MODULE, [Sup, SessionId, ServerModule,ConnectionReference], []).

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
init([Sup, SessionId, ServerModule, {TransportType, {Req, Index}}]) ->
    process_flag(trap_exit, true),
    PollingDuration = 
    case application:get_env(polling_duration) of
        {ok, Time} ->
            Time;
        _ ->
            error_logger:warning_report(
                "Could not load default polling_duration value from "
                "the application file. Setting the default value to 20000 ms."
            ),
            20000
    end,
    CloseTimeout = 
    case application:get_env(close_timeout) of
	{ok, Time0} ->
	    Time0;
	_ ->
	    8000
    end,
    {ok, EventMgr} = gen_event:start_link(),
    send_message(#msg{content = SessionId}, Req, Index, ServerModule, Sup),
    {ok, #state {
       session_id = SessionId,
       server_module = ServerModule,
       connection_reference = {TransportType, none},
       req = Req,
       index = Index,
       polling_duration = {make_ref(), PollingDuration},
       close_timeout = CloseTimeout,
       event_manager = EventMgr,
       sup = Sup
      }};

init([Sup, SessionId, ServerModule, {TransportType, Req}]) ->
    init([Sup, SessionId, ServerModule, {TransportType, {Req, undefined}}]).


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
handle_call({_TransportType, data, Req}, From, #state{ server_module = ServerModule,
                                                       event_manager = EventManager,
                                                       polling_duration = Interval,
                                                       sup = Sup } = State) ->
    Msgs = [socketio_data:decode(#msg{content=Data}) || {"data", Data} <- ServerModule:parse_post(Req)],
    {Response, NewState} =
    case cors_headers(ServerModule:get_headers(Req), Sup) of
        {false, _Headers} ->
            Reply = gen_server:reply(From, ServerModule:respond(Req, 405, "unauthorized")),
            {Reply, State};
        {_, Headers} ->
            F = fun(#heartbeat{}, _Acc) ->
                    {timer, reset_duration(Interval)};
                (M, Acc) ->
                    gen_event:notify(EventManager, {message, self(), M}),
                    Acc
            end,
            TmpState = case lists:foldl(F, undefined, lists:flatten(Msgs)) of
                {timer, NewInterval} ->
                    State#state{ polling_duration = NewInterval};
                undefined ->
                    State
            end,
            Reply = gen_server:reply(From, ServerModule:respond(Req, 200, [Headers | [{"Content-Type", "text/plain"}]], "ok")),
            {Reply, TmpState}
    end,
    {reply, Response, NewState};

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
%% Polling
handle_cast({TransportType, polling_request, {Req, Index}, Server}, State) ->
    handle_cast({TransportType, polling_request, Req, Server}, State#state{ index = Index});
handle_cast({TransportType, polling_request, Req, Server}, #state { server_module = ServerModule,
                                                                    polling_duration = Interval,
                                                                    message_buffer = [] } = State) ->
    ServerModule:ensure_longpolling_request(Req),
    link(ServerModule:socket(Req)),
    {noreply, State#state{ connection_reference = {TransportType, connected}, req = Req,
                           caller = Server,
                           polling_duration = reset_duration(Interval) }};

handle_cast({TransportType, polling_request, Req, Server}, #state { server_module = ServerModule, 
                                                                    message_buffer = Buffer } = State) ->
    link(ServerModule:socket(Req)),
    handle_cast({send, {buffer, Buffer}}, State#state{ connection_reference = {TransportType, connected},
						       req = Req, caller = Server, message_buffer = []});

%% Send to client
handle_cast({send, Message}, #state{ connection_reference = {_TransportType, none}, message_buffer = Buffer } = State) ->
    {noreply, State#state{ message_buffer = lists:append(Buffer, [Message])}};

handle_cast({send, Message}, #state{ server_module = ServerModule,
                                     connection_reference = {TransportType, connected }, req = Req, caller = Caller,
                                     index = Index, sup = Sup, polling_duration = Interval} = State) ->
    gen_server:reply(Caller, send_message(Message, Req, Index, ServerModule, Sup)),
    {noreply, State#state{ connection_reference = {TransportType, none},
                           polling_duration = reset_duration(Interval) }};

handle_cast(_, State) ->
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
%% A client has disconnected. We fire a timer (CloseTimeout)!
handle_info({'EXIT',Connection,_Reason}, #state{ connection_reference = {TransportType, _ }, close_timeout = CloseTimeout} = State) when is_port(Connection);
																	 is_pid(Connection)->
    {noreply, State#state { connection_reference = {TransportType, none}}, CloseTimeout};

%% Connections has timed out, but is technically still active. This is like a
%% heartbeat, but for polling connections.
handle_info({timeout, _Ref, polling}, #state{ server_module = ServerModule,
                             connection_reference = {_TransportType, connected},
                             caller = Caller, req = Req, index = Index, sup = Sup } = State) ->
    gen_server:reply(Caller, send_message("", Req, Index, ServerModule, Sup)),
    {noreply, State};

%% Client has timed out, no active connection found. (connection_reference = none)
handle_info(timeout, #state{ server_module = ServerModule, caller = Caller, req = Req } = State) ->
    gen_server:reply(Caller, apply(ServerModule, respond, [Req, 200,""])),
    {stop, shutdown, State};

%% client has timed out, no active connection found, but the normal close_timeout
%% is being interrupted by the polling timeout timer interacting in here.
%% We defer to the preceding clause.
handle_info({timeout, _Ref, polling}, #state{ } = State) ->
    handle_info(timeout, State);

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
send_message(#msg{} = Message, Req, Index, ServerModule, Sup) ->
    send_message(socketio_data:encode(Message), Req, Index, ServerModule, Sup);

send_message({buffer, Messages}, Req, Index, ServerModule, Sup) ->
    Messages0 = lists:map(fun(M) ->
				  case M of
				      #msg{} ->
					  socketio_data:encode(M);
				      _ ->
					  M
				  end
			  end, Messages),
    send_message(Messages0, Req, Index, ServerModule, Sup);

send_message(Message, Req, undefined, ServerModule, Sup) ->
    Headers = [{"Connection", "keep-alive"}],
    Headers0 =
	case cors_headers(apply(ServerModule, get_headers, [Req]), Sup) of
	    {false, _} ->
		Headers;
	    {_, Headers1} ->
		[Headers1|Headers]
	end,
    apply(ServerModule, respond, [Req, 200, Headers0, Message]);

send_message(Message, Req, Index, ServerModule, Sup) ->
    Headers = [{"Connection", "keep-alive"}],
    case cors_headers(apply(ServerModule, get_headers, [Req]), Sup) of
	{false, _} ->
	    apply(ServerModule, respond, [Req, 200, "alert('Cross domain security restrictions not met');"]);
	{_, Headers0} ->
	    send_message_1([Headers0|Headers], Message, Req, Index, ServerModule)
	end.

send_message_1(Headers, Message, Req, Index, ServerModule) ->
    Headers0 = [{"Content-Type", "text/javascript; charset=UTF-8"}|Headers],
    Message0 = binary_to_list(jsx:term_to_json(list_to_binary(Message), [{strict, false}])),
    Message1 = "io.JSONP["++Index++"]._(" ++ Message0 ++ ");",
    apply(ServerModule, respond, [Req, 200, Headers0, Message1]).

cors_headers(Headers, Sup) ->
    case proplists:get_value('Origin', Headers) of
	undefined ->
	    {undefined, []};
	Origin ->
	    case socketio_listener:verify_origin(Origin, socketio_listener:origins(Sup)) of
		true ->
		    Headers0 = [{"Access-Control-Allow-Origin", "*"}],
		    Headers1 =
			case proplists:get_value('Cookie', Headers) of
			    undefined ->
				Headers0;
			    _Cookie ->
				[{"Access-Control-Allow-Credentials", "true"}|Headers0]
			end,
		    {true, [Headers1]};
		false ->
		    {false, Headers}
	    end
    end.

reset_duration({TimerRef, Time}) ->
    erlang:cancel_timer(TimerRef),
    NewRef = erlang:start_timer(Time, self(), polling),
    {NewRef, Time}.
