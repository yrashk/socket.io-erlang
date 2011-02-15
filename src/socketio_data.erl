-module(socketio_data).
-export([parse/2]).

-record(parser, 
		{
		  buf = [],
		  acc = [],
		  expr = message,
		  f
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

parse(Buf,Fun) when is_list(Buf), is_function(Fun) ->
	parse(#parser{ buf = Buf, f = Fun }).

parse(#parser{ expr = message, buf = [] }) ->
	ok;
parse(#parser{ expr = message, buf = [$,|T], acc = [] } = Parser) ->
	parse(Parser#parser{ expr = message, buf = T });
parse(#parser{ expr = message, buf = [H|T], acc = [] } = Parser) ->
	parse(Parser#parser{ expr = message_type, acc = [H], buf = T });

parse(#parser{ expr = message_type, buf = [$:|T], acc = Acc } = Parser) ->
	{Type,_} = string:to_integer(lists:reverse(Acc)),
	parse(Parser#parser{ expr = {message, Type}, buf = T, acc = [] });
parse(#parser{ expr = message_type, buf = [H|T], acc = Acc } = Parser) ->
	parse(Parser#parser{ expr = message_type, buf = T, acc = [H|Acc] });

parse(#parser{ expr = {message, Type}, buf = [$:|T], acc = Acc } = Parser) ->
	{Length,_} = string:to_integer(lists:reverse(Acc)),
	parse(Parser#parser{ expr = {message, Type, Length}, buf = T, acc = [] });
parse(#parser{ expr = {message, Type}, buf = [H|T], acc = Acc } = Parser) ->
	parse(Parser#parser{ expr = {message, Type}, buf = T, acc = [H|Acc] });

parse(#parser{ expr = {message, Type, 0}, buf = Buf, acc = Acc } = Parser) -> 
	parse(Parser#parser{ expr = {message, Type, 0, []}, buf = Buf, acc = Acc }); % end of message, jump to message production

parse(#parser{ expr = {message, Type, Length}, buf = [$:|T], acc = [] } = Parser) ->
	parse(Parser#parser{ expr = {message, Type, Length - 1, []}, buf = T });
parse(#parser{ expr = {message, Type, Length}, buf = [$:|T], acc = Acc } = Parser) when length(Acc) > 0 ->
	Anns = [{key, Acc}],
	parse(Parser#parser{ expr = {message, Type, Length - 1, Anns}, buf = T, acc = [] });

parse(#parser{ expr = {message, Type, Length}, buf = [H|T], acc = Acc } = Parser) ->
	parse(Parser#parser{ expr = {message, Type, Length - 1}, buf = T, acc = [H|Acc] });

parse(#parser{ expr = {message, Type, 0, Anns}, buf = [$,|T], acc = Acc, f = F } = Parser) ->
	F(message(Parser)),
	parse(Parser#parser{ expr = message, buf = T, acc = [] });

parse(#parser{ expr = {message, Type, Length, [{key, Key}|RestAnns]}, buf = [$:|T], acc = Acc } = Parser) ->
	NewAnn = {Key, lists:reverse(Acc)},
	parse(Parser#parser{ expr = {message, Type, Length - 1, [NewAnn|RestAnns]}, buf = T, acc = [] });
parse(#parser{ expr = {message, Type, Length, [{key, Key}|RestAnns]}, buf = [10|T], acc = Acc } = Parser) ->
	NewAnn = {Key, Acc},
	parse(Parser#parser{ expr = {message, Type, Length - 1, [NewAnn|RestAnns]}, buf = T, acc = [] });

parse(#parser{ expr = {message, Type, Length, Anns}, buf = [H|T], acc = Acc } = Parser) ->
	parse(Parser#parser{ expr = {message, Type, Length - 1, Anns }, buf = T, acc = [H|Acc] }).

message(#parser{ expr = {message, 0, _, _}, acc = _ }) ->
	disconnect;
message(#parser{ expr = {message, 1, _, Anns}, acc = Acc}) ->
	#msg{ anns = Anns, content = lists:reverse(Acc) };
message(#parser{ expr = {message, 2, _, _}, acc = Acc}) ->
	{Index, _} = string:to_integer(lists:reverse(Acc)),
	#hearbeat{ index = Index };
message(#parser{ expr = {message, 3, _, _}, acc = Acc}) ->
	#session{ id = lists:reverse(Acc) }.


%% TESTS	
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
simple_disconnect_test() ->
	parse("0:0:,", fun (X) -> self() ! X end),
	receive X ->
			?assertMatch(disconnect, X)
	end.
simple_msg_test() ->
	parse("1:18:r:chat:Hello world,", fun (X) -> self() ! X end),
	receive X ->
			?assertMatch(#msg{ anns = [{"r","chat"}], content = "Hello world"}, X)
	end.

simple_heartbeat_test() ->
	parse("2:1:0,", fun (X) -> self() ! X end),
	receive X ->
			?assertMatch(#hearbeat{ index = 0 }, X)
	end.

simple_session_test() ->
	parse("3:3:253,", fun (X) -> self() ! X end),
	receive X ->
			?assertMatch(#session{ id = "253" }, X)
	end.

-endif.
