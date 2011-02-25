-module(socketio_data).
-include_lib("socketio.hrl").
-export([encode/1, parse/2, string_reader/2]).

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


%% Not sure if session messages are any different. All i know is they're the first message
%% that arrives from the client.
%% -record(session,
%% 	{
%% 	  id
%% 	}).

encode(#msg{ content = Content, json = false }) when is_list(Content) ->
    Length = integer_to_list(length(Content)),
    "~m~" ++ Length ++ "~m~" ++ Content;

encode(#msg{ content = Content, json = true }) ->
    JSON = binary_to_list(jsx:term_to_json(Content)),
    Length = integer_to_list(length(JSON) + 3),
    "~m~" ++ Length ++ "~m~~j~" ++ JSON;

encode(#heartbeat{ index = Index }) ->
    String = integer_to_list(Index),
    Length = integer_to_list(length(String) + 3),
    "~m~" ++ Length ++ "~m~~h~" ++ String.

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

% Parse the header
parse_1(#parser{ expr = Message, buf = [$~|T], acc = [] } = Parser) ->
    parse_1(Parser#parser{ expr = Message, buf = T});
parse_1(#parser{ expr = message, buf = [$~|T], acc = Length } = Parser) ->
    {Length0, _} = string:to_integer(lists:reverse(Length)), 
    parse_1(Parser#parser{ expr = {message, Length0, controller}, buf = T, acc = [] });
parse_1(#parser{ expr = {message, _Length, _Type} = Message, buf = [$~|T] } = Parser) ->
    parse_1(Parser#parser{ expr = Message, buf = T, acc = [] });
parse_1(#parser{ expr = message, buf = [$m|T] } = Parser) ->
    parse_1(Parser#parser{ expr = message, buf = T});
parse_1(#parser{ expr = {message, _Length, controller}, buf = [$m|T] } = Parser) ->
    parse_1(Parser#parser{ expr = {message, _Length, controller}, buf = T});

% Message type
parse_1(#parser{ expr = {message, _, controller}, buf = [$h|T], acc = Acc} = Parser) -> % heartbeat
    parse_1(Parser#parser{ expr = {message, 0, 1, body}, buf = T, acc = Acc});
parse_1(#parser{ expr = {message, Length, controller}, buf = [$j|T], acc = Acc} = Parser) -> % json
    parse_1(Parser#parser{ expr = {message, Length, 2, body}, buf = T, acc = Acc});
parse_1(#parser{ expr = {message, Length, controller}, buf = [H|T], acc = Acc} = Parser) -> % regular
    parse_1(Parser#parser{ expr = {message, Length, 0, body}, buf = T, acc = [H|Acc]});

% Rest
parse_1(#parser{ expr = Message, buf = [H|T], acc = Acc} = Parser) ->
    parse_1(Parser#parser{ expr = Message, buf = T, acc = [H|Acc]});

parse_1(#parser{ expr = {message, Length, Type, body}, f = F } = Parser) ->
    F(message(Parser#parser{expr = {message, Length, Type}})).

message(#parser { expr = {message, Length, 0}, acc = Acc}) ->
    #msg{ content = lists:reverse(Acc), length = Length };
message(#parser{ expr = {message, 0, 1}, acc = Acc}) ->
    {Index, _} = string:to_integer(lists:reverse(Acc)),
    #heartbeat{ index = Index };
message(#parser{ expr = {message, Length, 2}, acc = Acc}) ->
    #msg{ content = jsx:json_to_term(list_to_binary(lists:reverse(Acc))), json = true, length = Length }.

string_reader(#parser{ reader = undefined } = Parser, String) ->
    {String, Parser#parser{ reader = String }};
string_reader(#parser{ reader = String } = Parser, String) ->
    {eof, Parser}.


%% TESTS
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
simple_msg_test() ->
    parse(fun (Parser) -> string_reader(Parser, "~m~11~m~Hello world") end, fun (X) -> self() ! X end),
    receive X ->
	    ?assertMatch(#msg{ content = "Hello world", length = 11}, X)
    end.

complex_msg_test() ->
    parse(fun (Parser) -> string_reader(Parser, "~m~11~m~Hello~world") end, fun (X) -> self() ! X end),
    receive X ->
	    ?assertMatch(#msg{ content = "Hello~world", length = 11}, X)
    end.

simple_heartbeat_test() ->
    parse(fun (Parser) -> string_reader(Parser, "~m~1~m~h~0") end, fun (X) -> self() ! X end),
    receive X ->
	    ?assertMatch(#heartbeat{ index = 0 }, X)
    end.

simple_json_test() ->
    parse(fun (Parser) -> string_reader(Parser, "~m~20~m~~j~{\"hello\":\"world\"}") end, fun (X) -> self() ! X end),
    receive X ->
	    ?assertMatch(#msg{ content = [{<<"hello">>,<<"world">>}], json = true, length = _ }, X)
    end.

json_encoding_test() ->
    JSON = [{<<"hello">>,<<"world">>}],
    Msg = #msg{ content = JSON, json = true, length = 17 },
    Data = encode(Msg),
    parse(fun (Parser) -> string_reader(Parser, Data) end, fun (X) -> self() ! X end),
    receive X ->
	    ?assertMatch(#msg{ content = JSON, json = true, length = _ }, X)
    end.
   

-endif.
