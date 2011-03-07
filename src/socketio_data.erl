-module(socketio_data).
-include_lib("socketio.hrl").
-export([encode/1, decode/1]).

%%%%%%%%%%%%%%%%%%%%
%%% The Sockets.js server-side protocol as processed by https://github.com/LearnBoost/Socket.IO-node/blob/master/lib/socket.io/utils.js
%%%%%%%%%%%%%%%%%%%%
%%
%% Frame: ~m~
%% Message: some string
%% JSON Message: ~j~ ++ String Version of JSON object
%% Heartbeat: ~h~ ++ Index
%% Result: Frame ++ Length of Message ++ Frame ++ Message
-define(FRAME, "~m~").
-define(JSON_FRAME, "~j~").
-define(HEARTBEAT_FRAME, "~h~").


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

decode(#msg{content=Str}) when is_list(Str) ->
    header(Str).

header(?FRAME ++ Rest) ->
    header(Rest, []).

header(?FRAME ++ Rest=[_|_], Acc)->
    Length = list_to_integer(lists:reverse(Acc)),
    body(Length, Rest);
header([N|Rest], Acc) when N >= $0, N =< $9 ->
    header(Rest, [N|Acc]).

body(Length, ?JSON_FRAME++Body) ->
    json(Length-3, Body);
body(Length, ?HEARTBEAT_FRAME++Body) ->
    heartbeat(Length-3, Body, []);
body(Length, Body) ->
    #msg{content=lists:sublist(Body, Length)}.

json(Length, Body) ->
    Object = lists:sublist(Body, Length),
    #msg{content=jsx:json_to_term(list_to_binary(Object)), json=true}.

heartbeat(0, _, Acc) -> #heartbeat{index=list_to_integer(lists:reverse(Acc))};
heartbeat(Length, [N|Rest], Acc) when N >= $0, N =< $9 ->
    heartbeat(Length-1, Rest, [N|Acc]).


%% TESTS
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
%% For more reliable tests, see the proper module in tests/prop_transport.erl

simple_msg_test() ->
    X = decode(#msg{content="~m~11~m~Hello world"}),
    ?assertMatch("Hello world", X#msg.content).

complex_msg_test() ->
    X = decode(#msg{content="~m~11~m~Hello~world"}),
    ?assertMatch("Hello~world", X#msg.content).

simple_heartbeat_test() ->
    X = decode(#msg{content="~m~4~m~~h~0"}),
    ?assertMatch(0, X#heartbeat.index).

simple_json_test() ->
    X = decode(#msg{content="~m~20~m~~j~{\"hello\":\"world\"}"}),
    ?assertMatch(#msg{content=[{<<"hello">>,<<"world">>}], json=true}, X).

json_encoding_test() ->
    JSON = [{<<"hello">>,<<"world">>}],
    Msg = #msg{content = JSON, json=true},
    Data = encode(Msg),
    X = decode(#msg{content=Data}),
    ?assertMatch(#msg{content=JSON, json=true}, X).

-endif.
