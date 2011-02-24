#! /usr/bin/env escript
%%! -pa ../ebin ../deps/misultin/ebin
-mode(compile).

main(_) ->
    misultin:start_link([{port, 7878}, {loop, fun(Req) -> handle_http(Req) end}]),
    receive _ -> ok end.

handle_http(Req) ->
    handle_http_1(Req:get(uri), Req).

handle_http_1({abs_path, "/socket.io.js"}, Req) ->
    Req:file(filename:join([filename:dirname(code:which(?MODULE)), "..", "deps", "Socket.IO", "socket.io.js"]));
handle_http_1({abs_path, "/"}, Req) ->
    Req:file(filename:join([filename:dirname(code:which(?MODULE)), "index.html"]));
handle_http_1({abs_path, Path}, Req) ->
    io:format("~p~n", [{Path, Req}]),
    Req:respond(200).




