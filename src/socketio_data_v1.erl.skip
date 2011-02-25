-module(socketio_data_v1).
-export([parse/2, string_reader/2]).

-record(parser,
        {
          %% function that yields content
          reader_fun,
          acc = [],
          expr = message,
          %% callback
          f,
          %% internal state of the reader
          reader,
          buf = []
        }).

-record(msg,
                {
                  anns = [],
                  content = []
                }).

-record(hearbeat,
                {
                  index
                }).

-record(session,
                {
                  id
                }).

parse(Reader, Fun) when is_function(Reader), is_function(Fun) ->
        parse(#parser{ reader_fun = Reader, f = Fun }).

parse(#parser{ reader_fun = Reader } = Parser) ->
        case Reader(Parser) of
            {eof, _} ->
                ok;
            {Buffer, Parser1} ->
                parse_1(Parser1#parser{ buf = Buffer }),
                parse(Parser1)
        end.

parse_1(#parser{ expr = message, buf = [] }) ->
        ok;
parse_1(#parser{ expr = message, buf = [$,|T], acc = [] } = Parser) ->
        parse_1(Parser#parser{ expr = message, buf = T });
parse_1(#parser{ expr = message, buf = [H|T], acc = [] } = Parser) ->
        parse_1(Parser#parser{ expr = message_type, acc = [H], buf = T });

parse_1(#parser{ expr = message_type, buf = [$:|T], acc = Acc } = Parser) ->
        {Type,_} = string:to_integer(lists:reverse(Acc)),
        parse_1(Parser#parser{ expr = {message, Type}, buf = T, acc = [] });
parse_1(#parser{ expr = message_type, buf = [H|T], acc = Acc } = Parser) ->
        parse_1(Parser#parser{ expr = message_type, buf = T, acc = [H|Acc] });

parse_1(#parser{ expr = {message, Type}, buf = [$:|T], acc = Acc } = Parser) ->
        {Length,_} = string:to_integer(lists:reverse(Acc)),
        parse_1(Parser#parser{ expr = {message, Type, Length}, buf = T, acc = [] });
parse_1(#parser{ expr = {message, Type}, buf = [H|T], acc = Acc } = Parser) ->
        parse_1(Parser#parser{ expr = {message, Type}, buf = T, acc = [H|Acc] });

parse_1(#parser{ expr = {message, Type, 0}, buf = Buf, acc = Acc } = Parser) ->
        parse_1(Parser#parser{ expr = {message, Type, 0, []}, buf = Buf, acc = Acc }); % end of message, jump to message production

parse_1(#parser{ expr = {message, Type, Length}, buf = [$:|T], acc = [] } = Parser) ->
        parse_1(Parser#parser{ expr = {message, Type, Length - 1, []}, buf = T });
parse_1(#parser{ expr = {message, Type, Length}, buf = [$:|T], acc = Acc } = Parser) when length(Acc) > 0 ->
        Anns = [{key, Acc}],
        parse_1(Parser#parser{ expr = {message, Type, Length - 1, Anns}, buf = T, acc = [] });

parse_1(#parser{ expr = {message, Type, Length}, buf = [H|T], acc = Acc } = Parser) ->
        parse_1(Parser#parser{ expr = {message, Type, Length - 1}, buf = T, acc = [H|Acc] });

parse_1(#parser{ expr = {message, _Type, 0, _Anns}, buf = [$,|T], acc = _Acc, f = F } = Parser) ->
        F(message(Parser)),
        parse_1(Parser#parser{ expr = message, buf = T, acc = [] });

parse_1(#parser{ expr = {message, Type, Length, [{key, Key}|RestAnns]}, buf = [$:|T], acc = Acc } = Parser) ->
        NewAnn = {Key, lists:reverse(Acc)},
        parse_1(Parser#parser{ expr = {message, Type, Length - 1, [NewAnn|RestAnns]}, buf = T, acc = [] });
parse_1(#parser{ expr = {message, Type, Length, [{key, Key}|RestAnns]}, buf = [10|T], acc = Acc } = Parser) ->
        NewAnn = {Key, Acc},
        parse_1(Parser#parser{ expr = {message, Type, Length - 1, [NewAnn|RestAnns]}, buf = T, acc = [] });

parse_1(#parser{ expr = {message, Type, Length, Anns}, buf = [H|T], acc = Acc } = Parser) ->
        parse_1(Parser#parser{ expr = {message, Type, Length - 1, Anns }, buf = T, acc = [H|Acc] }).

message(#parser{ expr = {message, 0, _, _}, acc = _ }) ->
        disconnect;
message(#parser{ expr = {message, 1, _, Anns}, acc = Acc}) ->
        #msg{ anns = Anns, content = lists:reverse(Acc) };
message(#parser{ expr = {message, 2, _, _}, acc = Acc}) ->
        {Index, _} = string:to_integer(lists:reverse(Acc)),
        #hearbeat{ index = Index };
message(#parser{ expr = {message, 3, _, _}, acc = Acc}) ->
        #session{ id = lists:reverse(Acc) }.


string_reader(#parser{ reader = undefined } = Parser, String) ->
    {String, Parser#parser{ reader = String }};
string_reader(#parser{ reader = String } = Parser, String) ->
    {eof, Parser}.


%% TESTS
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
simple_disconnect_test() ->
        parse(fun (Parser) -> string_reader(Parser, "0:0:,") end, fun (X) -> self() ! X end),
        receive X ->
                        ?assertMatch(disconnect, X)
        end.
simple_msg_test() ->
        parse(fun (Parser) -> string_reader(Parser, "1:18:r:chat:Hello world,") end, fun (X) -> self() ! X end),
        receive X ->
                        ?assertMatch(#msg{ anns = [{"r","chat"}], content = "Hello world"}, X)
        end.

simple_heartbeat_test() ->
        parse(fun (Parser) -> string_reader(Parser, "2:1:0,") end, fun (X) -> self() ! X end),
        receive X ->
                        ?assertMatch(#hearbeat{ index = 0 }, X)
        end.

simple_session_test() ->
        parse(fun (Parser) -> string_reader(Parser, "3:3:253,") end, fun (X) -> self() ! X end),
        receive X ->
                        ?assertMatch(#session{ id = "253" }, X)
        end.

-endif.
