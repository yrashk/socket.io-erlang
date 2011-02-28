-module(socketio_transport_websocket_tests).
-include_lib("eunit/include/eunit.hrl").

transport_websocket_test_() ->
    socketio_transport_tests:transport_tests("websocket").
