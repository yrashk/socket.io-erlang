-module(socketio_client_tests).
-include_lib("../include/socketio.hrl").
-include_lib("eunit/include/eunit.hrl").
-behaviour(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).
-export([handle_request/2]).

-ifdef(TEST).
t_echoes_string({Client, EventMgr}) ->
    gen_event:notify(EventMgr, {change_pid, self()}),
    socketio_client:send(Client, #msg{ content = "some text"}),
    receive 
        X ->
            ?assertEqual("some text", X)
    end.

t_echoes_json({Client, EventMgr}) ->
    gen_event:notify(EventMgr, {change_pid, self()}),
    JSON = [{<<"key">>,<<"value">>}],
    socketio_client:send(Client, #msg{ content = JSON, json = true }),
    receive 
        X ->
            ?assertEqual(JSON, X)
    end.

t_session_id({Client, EventMgr}) ->
    gen_event:notify(EventMgr, {change_pid, self()}),
    socketio_client:send(Client, #msg{ content = "socketio_session" }),
    receive 
        X ->
            ?assertEqual(socketio_client:session_id(Client), X)
    end.

socketio_client_websocket_test_() ->
        {inorder,
         {foreach,
          fun () ->
                  error_logger:delete_report_handler(error_logger_tty_h), %% suppress annoying kernel logger
                  application:start(socketio),
                  {ok, _Pid, EventMgr} = socketio_listener:start([{http_port, 8989}, 
                                                                  {default_http_handler, ?MODULE}]),
                  ok = gen_event:add_handler(EventMgr, ?MODULE,[self()]),
                  ?cmd("open -a \"Google Chrome\" -g http://localhost:8989/"), %% FIXME: will only work on OSX
                  receive
                      {connected, Client, EM} -> 
                          {Client, EM}
                  end
          end,
          fun ({Client, _}) ->
                  socketio_client:send(Client, #msg{ content = "socketio_close" }),
                  application:stop(socketio)
          end,
          [fun (P) ->
                   [
                    ?_test(t_echoes_string(P)),
                    ?_test(t_echoes_json(P)),
                    ?_test(t_session_id(P))
                   ]
           end]}}.


handle_request({abs_path, "/"}, Req) ->
    Req:ok([{"Content-Type", "text/html"}], 
           "<html><head><script src=\"/socket.io.js\"></script>"
           "<script type=\"text/javascript\">"
           "function init() { \n"
           "socket = new io.Socket('localhost', {transports: ['websocket']});\n"
           
           "socket.on('message',function(data){\n"
           "if (data=='socketio_close') { window.close(); }\n"
           "if (data=='socketio_session') { socket.send(socket.transport.sessionid) } else {\n"
           "socket.send(data);\n"
           "}\n"
           "});\n"

           %% "socket.on('connect',function(){\n"
           %% "});\n"
           "socket.connect();\n"
           "}"
           "</script>"
           "</head><body onLoad=\"init()\"></body></html>");

handle_request({abs_path, _Path}, Req) ->
    Req:respond(200).

%% gen_event
init([Pid]) ->
    {ok, Pid}.

handle_event({change_pid, Pid}, _) ->
    {ok, Pid};

handle_event({client, Client}, Pid) ->
    EventMgr = socketio_client:event_manager(Client),
    ok = gen_event:add_handler(EventMgr, ?MODULE,[Pid]),
    Pid ! {connected, Client, EventMgr},
    {ok, Pid};

handle_event({disconnect, _Pid}, State) ->
    {ok, State};

handle_event({message, _Client, #msg{ content = Content } = _Msg}, Pid) ->
    Pid ! Content,
    {ok, Pid};

handle_event(V, Pid) ->
    ?debugVal(V),
    {ok, Pid}.


handle_call(_, State) ->
    {reply, ok, State}.

handle_info(_, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
-endif.
