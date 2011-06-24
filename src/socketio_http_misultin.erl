-module(socketio_http_misultin).
-behaviour(socketio_http_server).
-export([start_link/1, file/2, respond/2, respond/3, respond/4, parse_post/1, headers/2, chunk/2, stream/2,
         socket/1, get_headers/1, websocket_send/2, ensure_longpolling_request/1]).

start_link(Opts) ->
    Port = proplists:get_value(port, Opts),
    HttpProcess = proplists:get_value(http_process, Opts),
    Resource = proplists:get_value(resource, Opts),
    SSL = proplists:get_value(ssl, Opts),
    create_options(Port, HttpProcess, Resource, SSL),
    misultin:start_link(create_options(Port, HttpProcess, Resource, SSL)).

file(Request, Filename) ->
    misultin_req:file(Filename, Request).

respond(Request, Code) ->
    misultin_req:respond(Code, Request).

respond(Request, Code, Content) ->
    misultin_req:respond(Code, Content, Request).

respond(Request, Code, Headers, Content) ->
    misultin_req:respond(Code, Headers, Content, Request).

parse_post(Request) ->
    misultin_req:parse_post(Request).

headers(Request, Headers) ->
    misultin_req:stream(head, 200, Headers, Request).

chunk(Request, Chunk) ->
    misultin_req:chunk(Chunk, Request).

stream(Request, Data) ->
    misultin_req:stream(Data, Request).

socket(Request) ->
    get_socket(misultin_req:get(socket, Request)).

get_socket({sslsocket, new_ssl, Pid}) ->
    Pid;
get_socket(Socket) ->
    Socket.

get_headers(Request) ->
    misultin_req:get(headers, Request).

websocket_send(Ws, Data) ->
    misultin_ws:send(Data, Ws).

ensure_longpolling_request(Request) ->
    misultin_req:options([{comet, true}], Request).

%% Internal functions
create_options(Port, HttpProcess, Resource, undefined) ->
    [{port, Port},
     {name, false},
     {loop, fun (Req) -> handle_http(HttpProcess, Req) end},
     {ws_loop, fun (Ws) -> handle_websocket(HttpProcess, Resource, Ws) end},
     {ws_autoexit, false}];
create_options(Port, HttpProcess, Resource, SSL) ->
    Certfile = proplists:get_value(certfile, SSL),
    Keyfile = proplists:get_value(keyfile, SSL),
    Password = proplists:get_value(password, SSL),
    [{port, Port},
     {name, false},
     {loop, fun (Req) -> handle_http(HttpProcess, Req) end},
     {ws_loop, fun (Ws) -> handle_websocket(HttpProcess, Resource, Ws) end},
     {ws_autoexit, false},
     {ssl, [{certfile, Certfile},
	    {keyfile, Keyfile},
	    {password, Password}
	   ]}].

handle_http(Server, Req) ->
    Path = misultin_req:resource([urldecode], Req),
    gen_server:call(Server, {request, misultin_req:get(method, Req), lists:reverse(Path), Req}, infinity).

handle_websocket(Server, Resource, Ws) ->
    WsPath = misultin_ws:get(path, Ws),
    WsResource = string:tokens(WsPath,"/"),
    handle_websocket_1(Server, Resource, lists:reverse(WsResource), Ws).

handle_websocket_1(Server, Resource, ["flashsocket"|Resource], Ws) ->
    handle_websocket_1(Server, Resource, ["websocket"|Resource], Ws);

handle_websocket_1(Server, Resource, ["websocket"|Resource], Ws) ->
    {SessionID, Pid} = gen_server:call(Server, {session, generate, {websocket, Ws}, socketio_transport_websocket}),
    handle_websocket(Server, Ws, SessionID, Pid);
handle_websocket_1(_Server, _Resource, _WsResource, _Ws) ->
    ignore. %% FIXME: pass it through to the end user?

handle_websocket(Server, Ws, SessionID, Pid) ->
    receive
        {browser, Data} ->
            gen_server:call(Pid, {websocket, Data, Ws}),
            handle_websocket(Server, Ws, SessionID, Pid);
        closed ->
            gen_server:call(Pid, stop);
        _Ignore ->
            handle_websocket(Server, Ws, SessionID, Pid)
    end.

