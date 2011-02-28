-module(socketio_transport_websocket_tests).
-include_lib("eunit/include/eunit.hrl").

transport_websocket_test_() ->
    socketio_transport_tests:transport_tests("websocket").

transport_xhr_polling_test_() ->
    socketio_transport_tests:transport_tests("xhr-polling").
