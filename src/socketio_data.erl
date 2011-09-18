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
-define(JSON_FRAME_LENGTH, 3).
-define(HEARTBEAT_FRAME, "~h~").
-define(HEARTBEAT_FRAME_LENGTH, 3).


encode(#msg{ content = Content, json = false }) when is_list(Content) ->
    Length = integer_to_list(ux_string:length(Content)),
    ?FRAME ++ Length ++ ?FRAME ++ Content;

encode(#msg{ content = Content, json = true }) ->
    JSON = unicode:characters_to_list(jsx:term_to_json(Content)),
    Length = integer_to_list(ux_string:length(JSON) + ?JSON_FRAME_LENGTH),
    ?FRAME ++ Length ++ ?FRAME ++ ?JSON_FRAME ++ JSON;

encode(#heartbeat{ index = Index }) ->
    String = integer_to_list(Index),
    Length = integer_to_list(ux_string:length(String) + ?HEARTBEAT_FRAME_LENGTH),
    ?FRAME ++ Length ++ ?FRAME ++ ?HEARTBEAT_FRAME ++ String.

decode(#msg{content=Str}) when is_list(Str) ->
    header(Str).

header(?FRAME ++ Rest) ->
    header(Rest, []).

header(?FRAME ++ Rest=[_|_], Acc)->
    Length = list_to_integer(lists:reverse(Acc)),
    body(Length, ux_gb:split('extended',Rest));
header([N|Rest], Acc) when N >= $0, N =< $9 ->
    header(Rest, [N|Acc]).

body(Length, ?JSON_FRAME++Body) ->
    json(Length-?JSON_FRAME_LENGTH, Body, []);
body(Length, ?HEARTBEAT_FRAME++Body) ->
    heartbeat(Length-?HEARTBEAT_FRAME_LENGTH, Body);
body(Length, Body) ->
    message(Length, Body, []).

%% Unicode length is based on the functions an algorithm of 'ux'
json(0, Rest, Acc) ->
    JSONBin = unicode:characters_to_binary(lists:reverse(Acc)),
    [#msg{content=jsx:json_to_term(JSONBin, [{strict, false}]), json=true} |
     handle_rest(Rest)];
json(Length, ['x', Char | Rest], Acc) ->
    json(Length, Rest, [Char|Acc]);
json(Length, [Char|Rest], Acc) ->
    json(Length-1, Rest, [Char|Acc]).

heartbeat(Length, Body) ->
    {Heart, Rest} = lists:split(Length, Body),
    [#heartbeat{index=list_to_integer(Heart)} | handle_rest(Rest)].

%% It is possible that because of the way strings are put together
%% that the length is unreliable. The last character might be seen
%% as a control character without being one. Because of this,
%% we must make exceptions out of last-position unicode and make
%% sure they're included in the string.
message(0, ['x', Char | Rest], Acc) ->
    message(0, Rest, [Char|Acc]);
%% Now proceed as normal!
message(0, Rest, Acc) ->
    [#msg{content=lists:reverse(Acc)} | handle_rest(Rest)];
message(Length, ['x', Char | Rest], Acc) ->
    message(Length, Rest, [Char|Acc]);
message(Length, [Char|Rest], Acc) ->
    message(Length-1, Rest, [Char|Acc]).

handle_rest([]) -> [];
handle_rest(X) ->
    decode(#msg{content=strip(X)}).

%% Strip 'x' from the length stuff. We need to
%% because if many messages were buffered, we added
%% a bunch of 'x'es in location-sensitive areas with
%% ux_gb:strip/2. For example, [$~, UnicodeX, UnicodeY]
%% might give a control character before UnicodeX while
%% [UnicodeX, UnicodeY] might give none.
%% We have to remove them and then recalculate the length
%% again based on the string without the prefixed ~.
strip([]) -> [];
strip(['x', C | Rest]) ->
    [C | strip(Rest)];
strip([H|T]) ->
    [H | strip(T)].

%% TESTS
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
%% For more reliable tests, see the proper module in tests/prop_transport.erl

simple_msg_test_() ->
    [X] = decode(#msg{content="~m~11~m~Hello world"}),
    ?_assertMatch("Hello world", X#msg.content).

complex_msg_test_() ->
    [X] = decode(#msg{content="~m~11~m~Hello~world"}),
    ?_assertMatch("Hello~world", X#msg.content).

simple_heartbeat_test_() ->
    [X] = decode(#msg{content="~m~4~m~~h~0"}),
    ?_assertMatch(0, X#heartbeat.index).

simple_json_test_() ->
    [X] = decode(#msg{content="~m~20~m~~j~{\"hello\":\"world\"}"}),
    ?_assertMatch(#msg{content=[{<<"hello">>,<<"world">>}], json=true}, X).

json_encoding_test_() ->
    JSON = [{<<"hello">>,<<"world">>}],
    Msg = #msg{content = JSON, json=true},
    Data = encode(Msg),
    [X] = decode(#msg{content=Data}),
    ?_assertMatch(#msg{content=JSON, json=true}, X).

json_unicode_test_() ->
    %% E acute composition:
    Eacute = [16#0065,16#0301],
    % BinEacute = <<101,204,129>>,
    [X] = decode(#msg{content="~m~20~m~~j~{\"h"++Eacute++"llo\":\"w"++Eacute++"rld\"}"}),
    ?_assertMatch(#msg{content=[{<<$h,101,204,129,$l,$l,$o>>,
                                 <<$w,101,204,129,$r,$l,$d>>}], json=true},
                  X).

msg_unicode_test_() ->
    %% E acute composition:
    Eacute = [16#0065,16#0301],
    [X] = decode(#msg{content="~m~11~m~H"++Eacute++"llo~w"++Eacute++"rld"}),
    ?_assertEqual("H"++Eacute++"llo~w"++Eacute++"rld", X#msg.content).


-endif.
