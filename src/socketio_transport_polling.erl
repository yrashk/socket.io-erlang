-module(socketio_transport_polling).
-include_lib("../include/socketio.hrl").
-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
          session_id,
          message_buffer = [],
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
start_link(Sup, SessionId, ConnectionReference) ->
    gen_server:start_link(?MODULE, [Sup, SessionId, ConnectionReference], []).

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
init([Sup, SessionId, {TransportType, {Req, Index}}]) ->
    process_flag(trap_exit, true),
    PollingDuration = 
    case application:get_env(polling_duration) of
        {ok, Time} ->
            Time;
        _ ->
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
    send_message(#msg{content = SessionId}, Req, Index, Sup),
    {ok, #state {
       session_id = SessionId,
       connection_reference = {TransportType, none},
       req = Req,
       index = Index,
       polling_duration = PollingDuration,
       close_timeout = CloseTimeout,
       event_manager = EventMgr,
       sup = Sup
      }};

init([Sup, SessionId, {TransportType, Req}]) ->
    init([Sup, SessionId, {TransportType, {Req, undefined}}]).


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
handle_call({_TransportType, data, Req}, _From, #state{ event_manager = EventManager, sup = Sup } = State) ->
    Data = Req:parse_post(),
    Self = self(),
    Response =
	case cors_headers(Req:get(headers), Sup) of
	    {false, _Headers} ->
		Req:respond(405, "unauthorized");
	    {_, Headers0} ->
		Data = Req:parse_post(),
		Self = self(),
    		lists:foreach(fun({"data", M}) ->
    				      spawn(fun () ->
    						    F = fun(#heartbeat{}) -> ignore;
    							   (M0) -> gen_event:notify(EventManager, {message, Self,  M0})
    							end,
    						    F(socketio_data:decode(#msg{content=M}))
    					    end)
    			      end, Data),
		Req:ok([Headers0|[{"Content-Type", "text/plain"}]], "ok")
	end,
    {reply, Response, State};

%% Event management
handle_call(event_manager, _From, #state{ event_manager = EventMgr } = State) ->
    {reply, EventMgr, State};

%% Sessions
handle_call(session_id, _From, #state{ session_id = SessionId } = State) ->
    {reply, SessionId, State};

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
handle_cast({TransportType, polling_request, Req, Server}, #state { polling_duration = Interval, message_buffer = [] } = State) ->
    link(Req:get(socket)),
    {noreply, State#state{ connection_reference = {TransportType, connected}, req = Req, caller = Server }, Interval};

handle_cast({TransportType, polling_request, Req, Server}, #state { message_buffer = Buffer } = State) ->
    link(Req:get(socket)),
    handle_cast({send, {buffer, Buffer}}, State#state{ connection_reference = {TransportType, connected},
						       req = Req, caller = Server, message_buffer = []});

%% Send to client
handle_cast({send, Message}, #state{ connection_reference = {_TransportType, none}, message_buffer = Buffer } = State) ->
    {noreply, State#state{ message_buffer = lists:append(Buffer, [Message])}};

handle_cast({send, Message}, #state{ connection_reference = {TransportType, connected }, req = Req, caller = Caller, index = Index, sup = Sup} = State) -> %% FIXME: SOLUTION FOR BELOW IS TO MATCH ON INDEX=UNDEF HERE
    gen_server:reply(Caller, send_message(Message, Req, Index, Sup)),
    {noreply, State#state{ connection_reference = {TransportType, none}}};

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
handle_info({'EXIT',_Port,_Reason}, #state{ connection_reference = {TransportType, _ }, close_timeout = CloseTimeout} = State) when is_port(_Port) ->
    {noreply, State#state { connection_reference = {TransportType, none}}, CloseTimeout};

%% Connection has timed out
handle_info(timeout, #state{ connection_reference = {_TransportType, connected}, caller = Caller, req = Req, index = Index, sup = Sup } = State) ->
    gen_server:reply(Caller, send_message("", Req, Index, Sup)),
    {noreply, State};

%% Client has timed out
handle_info(timeout, #state{ caller = Caller } = State) ->
    gen_server:call(Caller, connection_gone),
    {stop, shutdown, State};

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
send_message(#msg{} = Message, Req, Index, Sup) ->
    send_message(socketio_data:encode(Message), Req, Index, Sup);

send_message({buffer, Messages}, Req, Index, Sup) ->
    Messages0 = lists:map(fun(M) ->
				  case M of
				      #msg{} ->
					  socketio_data:encode(M);
				      _ ->
					  M
				  end
			  end, Messages),
    send_message(Messages0, Req, Index, Sup);

send_message(Message, Req, undefined, Sup) ->
    Headers = [{"Connection", "keep-alive"}],
    Headers0 =
	case cors_headers(Req:get(headers), Sup) of
	    {false, _} ->
		Headers;
	    {_, Headers1} ->
		[Headers1|Headers]
	end,
    Req:ok(Headers0, Message);

send_message(Message, Req, Index, Sup) ->
    Headers = [{"Connection", "keep-alive"}],
    case cors_headers(Req:get(headers), Sup) of
	{false, _} ->
	    Req:ok("alert('Cross domain security restrictions not met');");
	{_, Headers0} ->
	    send_message_1([Headers0|Headers], Message, Req, Index)
	end.

send_message_1(Headers, Message, Req, Index) ->
    Headers0 = [{"Content-Type", "text/javascript; charset=UTF-8"}|Headers],
    Message0 = binary_to_list(jsx:term_to_json(list_to_binary(Message), [{strict, false}])),
    Message1 = "io.JSONP["++Index++"]._(" ++ Message0 ++ ");",
    Req:ok(Headers0, Message1).

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
