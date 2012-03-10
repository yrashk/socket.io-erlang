-module(socketio_client).
-include_lib("socketio.hrl").

%% API
-export([start_link/5, start/6]).
-export([event_manager/1, send/2, session_id/1, request/1]).

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
start_link(Sup, Module, SessionId, ServerModule, ConnectionReference) ->
    Module:start_link(Sup, SessionId, ServerModule, ConnectionReference).

start(Sup0, Module, SessionId, ServerModule, ConnectionReference, Port) ->
    Sup = gproc:lookup_local_name({socketio_client_sup, Port}),
    supervisor:start_child(Sup, [Sup0, Module, SessionId, ServerModule, ConnectionReference]).


send(Server, Message) ->
    gen_server:cast(Server, {send, Message}).

event_manager(Server) ->
    gen_server:call(Server, event_manager).

session_id(Server) ->
    gen_server:call(Server, session_id).

request(Server) ->
    gen_server:call(Server, req).
