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
	  reply_to,
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
init([Sup, SessionId, {'xhr-polling', Client}]) ->
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
    send_message(#msg{content = SessionId}, Client),
    {ok, #state{
       session_id = SessionId,
       connection_reference = {'xhr-polling', none},
       polling_duration = PollingDuration,
       close_timeout = CloseTimeout,
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
handle_call({'xhr-polling', data, Req}, _From, #state{ event_manager = EventManager } = State) ->
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
    Response = Req:ok("ok"),
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
handle_cast({'xhr-polling', polling_request, Req, Server}, #state { polling_duration = Interval, message_buffer = [] } = State) ->
    link(Req:get(socket)),
    {noreply, State#state{ connection_reference = {'xhr-polling', connected}, req = Req, reply_to = Server }, Interval};

handle_cast({'xhr-polling', polling_request, Req, Server}, #state { message_buffer = Buffer } = State) ->
    link(Req:get(socket)),
    gen_server:reply(Server, send_message({buffer, Buffer}, Req)),
    handle_cast({send, {buffer, Buffer}}, State#state{ connection_reference = {'xhr-polling', connected},
						       req = Req, reply_to = Server, message_buffer = [] });

%% Send
handle_cast({send, Message}, #state{ connection_reference = {'xhr-polling', none}, message_buffer = Buffer } = State) ->
    {noreply, State#state{ message_buffer = lists:append(Buffer, [Message])}};

handle_cast({send, Message}, #state{ connection_reference = {'xhr-polling', connected }, req = Req, reply_to = Caller} = State) ->
    gen_server:reply(Caller, send_message(Message, Req)),
    {noreply, State#state{ connection_reference = {'xhr-polling', none}}};

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
handle_info({'EXIT',_Port,_Reason}, #state{ connection_reference = {'xhr-polling', _ }, close_timeout = CloseTimeout} = State) when is_port(_Port) ->
    {noreply, State#state { connection_reference = {'xhr-polling', none}}, CloseTimeout};

%% Connection has timed out
handle_info(timeout, #state{ connection_reference = {'xhr-polling', connected}, reply_to = Caller, req = Req } = State) ->
    gen_server:reply(Caller, send_message("", Req)),
    {noreply, State};

%% Client has timed out
handle_info(timeout, State) ->
    {stop, shutdown, State};

handle_info(_Info, State) ->
    io:format("Got something ~p~n", [_Info]),
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
send_message(#msg{} = Message, Req) ->
    send_message(socketio_data:encode(Message), Req);

send_message({buffer, Messages}, Req) ->
    Messages0 = lists:map(fun(M) ->
				  case M of
				      #msg{} ->
					  socketio_data:encode(M);
				      _ ->
					  M
				  end
			  end, Messages),
    send_message(Messages0, Req);

send_message(Message, Req) -> %% ADD CORS
    Headers = [{"Connection", "keep-alive"}],
    Headers0 = case proplists:get_value('Referer', Req:get(headers)) of
		  undefined -> Headers;
		  Origin -> [{"Access-Control-Allow-Origin", Origin}|Headers]
	      end,
    Headers1 = case proplists:get_value('Cookie', Req:get(headers)) of
		   undefined -> Headers0;
		   _Cookie -> [{"Access-Control-Allow-Credentials", "true"}|Headers0]
	       end,
    send_message(Headers1, Message, Req).

send_message(Headers, Message, Req) ->
    Headers0 = [{"Content-Type", "text/plain"}|Headers],
    Req:ok(Headers0, Message).
