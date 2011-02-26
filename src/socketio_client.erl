-module(socketio_client).
-include_lib("socketio.hrl").

%% API
-export([start_link/3, start/4]).
-export([event_manager/1, send/2, session_id/1]).

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
start_link(Module, SessionId, ConnectionReference) ->
    Module:start_link(SessionId, ConnectionReference).

start(Sup0, Module, SessionId, ConnectionReference) ->
    Children = supervisor:which_children(Sup0),
    {Sup, _, _, _} = lists:keyfind(socketio_client_sup,1, Children),
    supervisor:start_child(Sup, [Module, SessionId, ConnectionReference]).


send(Server, Message) ->
    gen_server:cast(Server, {send, Message}).

event_manager(Server) ->
    gen_server:call(Server, event_manager).

session_id(Server) ->
    gen_server:call(Server, session_id).
