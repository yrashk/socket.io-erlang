-module(socketio_data_tests).
-include_lib("eunit/include/eunit.hrl").

%% Proper returns a list of failing tests.
prop_test() ->
    ?assertEqual([], proper:module(prop_transport)).
