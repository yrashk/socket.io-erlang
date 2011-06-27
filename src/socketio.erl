-module(socketio).

%% Application callbacks
-export([start/0, get_env/2]).

start() ->
  application:start(misultin),
  application:start(socketio).

get_env(Key, Default) ->
  case application:get_env(?MODULE, Key) of
    undefined -> Default;
    {ok, Value} -> Value
  end.