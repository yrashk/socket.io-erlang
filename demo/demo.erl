#! /usr/bin/env escript
%%! -pa ../ebin ../deps/misultin/ebin ../deps/ossp_uuid/ebin ../deps/jsx/ebin
-mode(compile).
-include_lib("../include/socketio.hrl").
-export([handle_request/2, handle_message/3]).

main(_) ->
    application:start(socketio),
    socketio_http:start(7878, ?MODULE, ?MODULE),
    receive _ -> ok end.

handle_request({abs_path, "/"}, Req) ->
    Req:file(filename:join([filename:dirname(code:which(?MODULE)), "index.html"]));
handle_request({abs_path, _Path}, Req) ->
    Req:respond(200).

handle_message(Server, SessionID, #msg{ content = Content } = Msg) ->
    io:format("Got a message: ~p from ~p~n",[Msg, SessionID]),
    gen_server:call(Server, {send, SessionID, #msg{ content = "hello!" }}),
    gen_server:call(Server, {send, SessionID, #msg{ content = [{<<"echo">>, Content}], json = true }}).


