-module(socketio_http_misultin).
-behaviour(socketio_http_server).
-export([start_link/1, file/2, respond/2, respond/3, respond/4, parse_post/1, headers/2, chunk/2, stream/2,
         socket/1, get_headers/1, websocket_send/2, ensure_longpolling_request/1]).

start_link(Opts) ->
    Port = proplists:get_value(port, Opts),
    HttpProcess = proplists:get_value(http_process, Opts),
    Resource = proplists:get_value(resource, Opts),
    misultin:start_link([{port, Port},
                         {name, false},
                         {loop, fun (Req) -> handle_http(HttpProcess, Req) end},
                         {ws_loop, fun (Ws) -> handle_websocket(HttpProcess, Resource, Ws) end},
                         {ws_autoexit, false}
                        ]).

file(Request, Filename) ->
    Request:file(Filename).

respond(Request, Code) ->
    Request:respond(Code).

respond(Request, Code, Content) ->
    Request:respond(Code, Content).

respond(Request, Code, Headers, Content) ->
    Request:respond(Code, Headers, Content).

parse_post(Request) ->
    Request:parse_post().

headers(Request, Headers) ->
    Request:stream(head, 200, Headers).

chunk(Request, Chunk) ->
    Request:chunk(Chunk).

stream(Request, Data) ->
    Request:stream(Data).

socket(Request) ->
    Request:get(socket).

get_headers(Request) ->
    Request:get(headers).

websocket_send(Ws, Data) ->
    Ws:send(Data).

ensure_longpolling_request(Request) ->
    Request:options([{comet, true}]).

%% Internal functions

handle_http(Server, Req) ->
    Path = Req:resource([urldecode]),
    gen_server:call(Server, {request, Req:get(method), lists:reverse(Path), Req}, infinity).

handle_websocket(Server, Resource, Ws) ->
    WsPath = Ws:get(path),
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

