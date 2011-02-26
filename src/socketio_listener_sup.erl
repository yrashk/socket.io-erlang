-module(socketio_listener_sup).

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

start_link(Options) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Options]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Options]) ->
    HttpPort = proplists:get_value(http_port, Options, 80),
    DefaultHttpHandler = proplists:get_value(default_http_handler, Options),
    {ok, EventMgr} = gen_event:start_link(),
    put(event_manager, EventMgr), %% this is ok because it is a write-once value
    {ok, { {one_for_one, 5, 10}, [
                                  {socketio_http, {socketio_http, start_link, [HttpPort,
                                                                               DefaultHttpHandler,
                                                                               EventMgr,
                                                                               self()]}, 
                                   permanent, 5000, worker, [socketio_http]},
                                  {socketio_client_sup, {socketio_client_sup, start_link, []}, 
                                   permanent, infinity, supervisor, [socketio_client_sup]}


                                 ]} }.

