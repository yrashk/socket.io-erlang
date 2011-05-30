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
    supervisor:start_link(?MODULE, [Options]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Options]) ->
    ServerModule = proplists:get_value(server, Options, socketio_http_misultin),
    HttpPort = proplists:get_value(http_port, Options, 80),
    DefaultHttpHandler = proplists:get_value(default_http_handler, Options),
    Resource = lists:reverse(string:tokens(proplists:get_value(resource, Options, "socket.io"),"/")),
    SSL = proplists:get_value(ssl, Options),
    Origins = proplists:get_value(origins,Options,[{"*", "*"}]),
    {ok, { {one_for_one, 5, 10}, [
                                  {socketio_listener_event_manager, {gen_event, start_link, []},
                                   permanent, 5000, worker, [gen_event]},

                                  {socketio_listener, {socketio_listener, start_link, [self(), Origins]},
                                   permanent, 5000, worker, [socketio_listener]},

                                  {socketio_http, {socketio_http, start_link, [ServerModule,
                                                                               HttpPort,
                                                                               Resource,
									       SSL,
                                                                               DefaultHttpHandler,
                                                                               self()]}, 
                                   permanent, 5000, worker, [socketio_http]},

                                  {socketio_client_sup, {socketio_client_sup, start_link, []}, 
                                   permanent, infinity, supervisor, [socketio_client_sup]}

                                 ]} }.

