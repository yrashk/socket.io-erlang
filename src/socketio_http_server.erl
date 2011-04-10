-module(socketio_http_server).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{start_link,1}, {file,2}, {respond,2}, {respond,3}, {respond,4},
     {parse_post,1}, {headers,2}, {chunk,2}, {stream,2},
     {socket,1},{get_headers,1}];
behaviour_info(_Other) ->
    undefined.
