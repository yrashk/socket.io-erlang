-module(socketio_listener_tests).

-include_lib("../include/socketio.hrl").
-include_lib("eunit/include/eunit.hrl").

empty_origins_test() ->
    ?assertEqual(false, socketio_listener:verify_origin("http://foo.bar",[])).

all_allowed_origins_test() ->
    ?assertEqual(true, socketio_listener:verify_origin("http://foo.bar",[{"a",80},{"*","*"}])).

host_allowed_origins_test() ->
    ?assertEqual(true, socketio_listener:verify_origin("http://foo.bar",[{"a",80},{"foo.bar","*"}])).

host_but_not_port_allowed_origins_test() ->
    ?assertEqual(false, socketio_listener:verify_origin("http://foo.bar:89",[{"a",80},{"foo.bar",80}])).

all_at_port_allowed_origins_test() ->
    ?assertEqual(true, socketio_listener:verify_origin("http://foo.bar:89",[{"a",80},{"*",89}])).

port_default_origins_test() ->
    ?assertEqual(true, socketio_listener:verify_origin("http://foo.bar",[{"a",80},{"*",80}])).


