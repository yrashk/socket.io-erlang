-module(socketio_listener).
-behaviour(gen_server).

-include_lib("ex_uri.hrl").

%% API
-export([start/1, server/1]).
-export([start_link/2]).
-export([event_manager/1, origins/1, origins/2]).
-export([verify_origin/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {
          sup,
          origins
         }).

%%%===================================================================
%%% API
%%%===================================================================

start(Options) ->
    case supervisor:start_child(socketio_listener_sup_sup, [Options]) of
        {ok, Pid} ->
            Children = supervisor:which_children(Pid),
            {_, Listener, _, _} = lists:keyfind(socketio_listener, 1, Children),
            {ok, Listener};
        {error,{already_started, Pid}} ->
            Children = supervisor:which_children(Pid),
            {_, Listener, _, _} = lists:keyfind(socketio_listener, 1, Children),
            {ok, Listener}
    end.

server(Sup) ->
    Children = supervisor:which_children(Sup),
    {_, Server, _, _} = lists:keyfind(socketio_listener, 1, Children),
    Server.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Sup, Origins) ->
    gen_server:start_link(?MODULE, [Sup, Origins], []).

event_manager(Server) ->
    gen_server:call(Server, event_manager).

origins(Server) ->
    gen_server:call(Server, origins).

origins(Server, Origins) ->
    gen_server:call(Server, {origins, Origins}).

verify_origin(Origin, Origins) ->
    {ok, #ex_uri{ authority = #ex_uri_authority{ host = Host, port = Port } } = _URI, _} = ex_uri:decode(Origin),
    verify_origin_1({Host, Port}, Origins).

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
init([Sup, Origins]) ->
    {ok, #state{
       sup = Sup,
       origins = Origins
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

%% Origins
handle_call(origins, _From, #state{ origins = Origins } = State) ->
    {reply, Origins, State};

handle_call({origins, Origins}, _From,State) ->
    {reply, Origins, State#state{ origins = Origins }};
            
%% Event management
handle_call(event_manager, _From, #state{ sup = Sup } = State) ->
    Children = supervisor:which_children(Sup),
    {_, EventMgr, _, _} = lists:keyfind(socketio_listener_event_manager, 1, Children),
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
verify_origin_1(_Origin, [{"*","*"}|_]) ->
    true;
verify_origin_1({Host, undefined}, Origins) ->
    verify_origin_1({Host, 80}, Origins);
verify_origin_1({_Host, Port}, [{"*", Port}|_]) ->
    true;
verify_origin_1({Host, _Port}, [{Host, "*"}|_]) ->
    true;
verify_origin_1({Host, Port}, [{Host, Port}|_]) ->
    true;
verify_origin_1(Origin, [_|Rest]) ->
    verify_origin_1(Origin, Rest);
verify_origin_1(_Origin, []) ->
    false.

