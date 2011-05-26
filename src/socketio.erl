-module(socketio).

%% Application callbacks
-export([start/0]).

start() ->
  application:start(misultin),
  application:start(socketio).