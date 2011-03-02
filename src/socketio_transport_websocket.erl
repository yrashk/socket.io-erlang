-module(socketio_transport_websocket).
-include_lib("socketio.hrl").
-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
          session_id,
          message_handler,
          connection_reference,
          heartbeats = 0,
          heartbeat_interval,
          event_manager
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
start_link(SessionId, ConnectionReference) ->
    gen_server:start_link(?MODULE, [SessionId, ConnectionReference], []).

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
init([SessionId, ConnectionReference]) ->
    HeartbeatInterval = 
    case application:get_env(heartbeat_interval) of
        {ok, Time} ->
            Time;
        _ ->
            infinity
    end,
    {ok, EventMgr} = gen_event:start_link(),
    socketio_client:send(self(), #msg{ content = SessionId }),
    gen_server:cast(self(), heartbeat),
    {ok, #state{
       session_id = SessionId,
       connection_reference = ConnectionReference,
       heartbeat_interval = HeartbeatInterval,
       event_manager = EventMgr
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

%% Websockets
handle_call({websocket, Data, _Ws}, _From, #state{ heartbeat_interval = Interval, event_manager = EventManager } = State) ->
    Self = self(),
    spawn_link(fun() ->
        F = fun (#heartbeat{}) -> ignore; %% FIXME: we should actually reply
                (M) -> gen_event:notify(EventManager, {message, Self,  M})
        end,
        F(socketio_data:decode(#msg{content=Data}))
    end),
    {reply, ok, State, Interval};

handle_call({websocket, _}, _From, State) ->
    {reply, ok, State};

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
%% Send
handle_cast({send, Message}, #state{ connection_reference = ConnectionReference, heartbeat_interval = Interval } = State) ->
    handle_send(ConnectionReference, Message),
    {noreply, State, Interval};

handle_cast(heartbeat, #state{ connection_reference = ConnectionReference, heartbeats = Beats,
                              heartbeat_interval = Interval } = State) ->
    Beats1 = Beats + 1,
    handle_send(ConnectionReference, #heartbeat{ index = Beats1 }),
    {noreply, State#state { heartbeats = Beats1 }, Interval}.

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
handle_info(timeout, State) ->
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
handle_send({websocket, Ws}, Message) ->
    Ws:send(socketio_data:encode(Message)).
