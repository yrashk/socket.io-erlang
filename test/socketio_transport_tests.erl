-module(socketio_transport_tests).
-include_lib("../include/socketio.hrl").
-include_lib("eunit/include/eunit.hrl").
-behaviour(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).
-export([handle_request/3]).
-export([transport_tests/1, transport_tests/2]).

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

transport_tests(Transport) ->
    transport_tests(firefox, Transport).

transport_tests(chrome, Transport) ->
    case os:type() of
       {unix,linux} ->
          transport_tests("google-chrome", Transport);
       _ ->
          transport_tests("open -a \"Google Chrome\" -g", Transport)
    end;
transport_tests(firefox, Transport) ->
    case os:type() of
       {unix,linux} ->
          transport_tests("firefox", Transport);
       _ ->
          transport_tests("open -a \"Firefox\"", Transport)
    end;

transport_tests(BrowserCommand, Transport) ->
        {inorder,
         {foreach,
          fun () ->
                  ets:new(socketio_tests, [public, named_table]),
                  ets:insert(socketio_tests, {transport, Transport}),
                  error_logger:delete_report_handler(error_logger_tty_h), %% suppress annoying kernel logger
                  application:start(misultin),
		                application:start(socketio),
                  {ok, Pid} = socketio_listener:start([{http_port, 8989}, 
                                                       {default_http_handler, ?MODULE}]),
                  EventMgr = socketio_listener:event_manager(Pid),
                  ok = gen_event:add_handler(EventMgr, ?MODULE,[self()]),
io:format("~nRunning: ~1024p~n", [BrowserCommand ++ " http://localhost:8989/"]),
                  ?cmd(BrowserCommand ++ " http://localhost:8989/"), %% FIXME: will only work on OSX/Linux
                  receive
                      {connected, Client, EM} -> 
                          {Client, EM};
                      Other ->
                          throw({unexpected_cmd_result, Other})
                  after
                      10000 ->
                          throw(browser_connect_timeout)
                  end
          end,
          fun ({Client, _}) ->
                  socketio_client:send(Client, #msg{ content = "socketio_close" }),
                  application:stop(socketio),
                  ets:delete(socketio_tests)
          end,
          [fun (P) ->
                   [
                    ?_test(t_echoes_string(P)),
                    ?_test(t_echoes_json(P)),
                    ?_test(t_session_id(P))
                   ]
           end]}}.


handle_request('GET', [], Req) ->
    Transports = 
        case ets:lookup(socketio_tests, transport) of
            [{transport, Transport}] ->
                "transports: ['" ++ Transport ++ "']";
            _ ->
                ""
        end,
    Req:ok([{"Content-Type", "text/html"}], 
           "<html><head><script src=\"/socket.io/socket.io.js\"></script>"
           "<script type=\"text/javascript\">"
           "function init() { \n"
           "socket = new io.Socket('localhost', {" ++ Transports ++ ", rememberTransport: false});\n"
           
           "socket.on('message',function(data){\n"
           "if (data=='socketio_close') { window.close(); } else \n"
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

handle_request(_, _, Req) ->
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
 
