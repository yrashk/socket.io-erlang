-module(socketio_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Port) ->
    supervisor:start_link(?MODULE, [Port]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Port]) ->
    gproc:add_local_name({?MODULE, Port}),
    {ok, { {simple_one_for_one, 5, 10}, [
                                  {socketio_client, {socketio_client, start_link, []}, 
                                   transient, 5000, worker, [socketio_client]}
                                 ]} }.

