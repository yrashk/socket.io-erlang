-module(socketio_data_v1).
-compile([export_all, {no_auto_import, [error/2]}]).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%
%%% The socket.io server-side protocol as processed by https://github.com/3rd-Eden/Socket.IO-node/blob/master/lib/parser.js
%%%%%%%%%%%%%

%% Framing is only given when no other suitable mode exists. In this case,
%% XHR-multipart will already frame these.
%% It is not defined whether we should 
-define(FRAME, 16#fffd).
-define(BINFRAME, <<65533>>).
-define(RESERVED_EVENTS, [<<"message">>, <<"connect">>, <<"disconnect">>,
                          <<"open">>, <<"close">>, <<"error">>, <<"retry">>,
                          <<"reconnect">>]).
disconnect("") -> <<"0">>;
disconnect(Endpoint) -> [<<"0::">>,Endpoint].

heartbeat() -> <<"2">>.

message(Id, EndPoint, Msg) when is_integer(Id) ->
    [<<"3:">>, integer_to_list(Id), $:, EndPoint, $:, Msg];
message(Id, EndPoint, Msg) when is_list(Id) ->
    [<<"3:">>, Id, $:, EndPoint, $:, Msg].

json(Id, EndPoint, Msg) when is_integer(Id) ->
    [<<"4:">>, integer_to_list(Id), $:, EndPoint, $:, jsx:term_to_json(Msg)];
json(Id, EndPoint, Msg) when is_list(Id) ->
    [<<"4:">>, Id, $:, EndPoint, $:, jsx:term_to_json(Msg)].

event(Id, EndPoint, Event, Msg) when is_integer(Id) ->
    case lists:member(iolist_to_binary(Event), ?RESERVED_EVENTS) of
        true -> erlang:error(badarg);
        false ->
            [<<"5:">>, integer_to_list(Id), $:, EndPoint, $:, Event,
                ?BINFRAME, jsx:term_to_json(Msg)]
    end;
event(Id, EndPoint, Event, Msg) when is_list(Id) ->
    case lists:member(iolist_to_binary(Event), ?RESERVED_EVENTS) of
        true -> erlang:error(badarg);
        false ->
            [<<"5:">>, Id, $:, EndPoint, $:, Event,
                ?BINFRAME, jsx:term_to_json(Msg)]
    end.

ack(Id) -> [<<"6:::">>, integer_to_list(Id)].

ack(Id, Data) ->
    [<<"6:::">>, integer_to_list(Id), $+, jsx:term_to_json(Data)].

error(EndPoint, Reason) ->
    [<<"7::">>, EndPoint, $:, Reason].

error(EndPoint, Reason, Advice) ->
    [<<"7::">>, EndPoint, $:, Reason, $+, Advice].

%%% PARSING

decode(<<"0">>) -> disconnect;
decode(<<"0::", EndPoint/binary>>) -> {disconnect, EndPoint};
%% Incomplete, needs to handle queries
decode(<<"1::", EndPoint/binary>>) -> {connect, EndPoint};
decode(<<"2">>) -> heartbeat;
decode(<<"3:", Rest/binary>>) ->
    {Id, R1} = id(Rest),
    {EndPoint, Data} = endpoint(R1),
    {message, Id, EndPoint, Data};
decode(<<"4:", Rest/binary>>) ->
    {Id, R1} = id(Rest),
    {EndPoint, Data} = endpoint(R1),
    {json, Id, EndPoint, jsx:json_to_term(Data)};
decode(<<"5:", Rest/binary>>) ->
    {Id, R1} = id(Rest),
    {EndPoint, R2} = endpoint(R1),
    {Event, JSON} = event(R2),
    {event, Id, EndPoint, Event, jsx:json_to_term(JSON)};
decode(<<"6:::", Rest/binary>>) ->
    case p_ack(Rest) of
        {Id, JSON} ->
            {ack, Id, jsx:json_to_term(JSON)};
        Id -> {ack, Id}
    end;
decode(<<"7::", Rest/binary>>) ->
    {EndPoint, R1} = endpoint(Rest),
    case reason(R1) of
        {Reason, Advice} ->
            {error, EndPoint, Reason, Advice};
        Reason ->
            {error, EndPoint, Reason}
    end.

id(X) -> id(X, "").

id(<<$:, Rest/binary>>, "") ->
    {"", Rest};
id(<<$:, Rest/binary>>, Acc) ->
    L = lists:reverse(Acc),
    {list_to_integer(L), Rest};
id(<<$+,$:, Rest/binary>>, Acc) when Acc =/= "" ->
    {lists:reverse([$+|Acc]), Rest};
id(<<Num, Rest/binary>>, Acc) when Num-$0 >= 0, Num-$0 =< 9 ->
    id(Rest, [Num|Acc]).

endpoint(X) -> endpoint(X,"").

endpoint(<<$:, Rest/binary>>, Acc) -> {lists:reverse(Acc), Rest};
endpoint(<<X, Rest/binary>>, Acc) -> endpoint(Rest, [X|Acc]).

event(X) ->
    [Event, JSON] = binary:split(X, ?BINFRAME),
    {Event, JSON}.

p_ack(X) ->
    case binary:split(X, <<"+">>) of
        [H] -> list_to_integer(binary_to_list(H));
        [H|T] -> list_to_tuple([list_to_integer(binary_to_list(H))|T])
    end.

reason(X) ->
   case list_to_tuple(binary:split(X, <<"+">>)) of
       {E} -> E;
       T -> T
   end.

%%% Tests based off the examples on the page
%%% ENCODING
disconnect_test_() ->
    [?_assertEqual(<<"0::/test">>, iolist_to_binary(disconnect("/test"))),
     ?_assertEqual(<<"0">>, iolist_to_binary(disconnect("")))].

connect_test_() -> []. % Only need to read, never to encode

%% No format specified in the spec.
heartbeat_test() ->
    ?assertEqual(<<"2">>, heartbeat()).

message_test_() ->
    [?_assertEqual(<<"3:1::blabla">>, iolist_to_binary(message(1, "", "blabla"))),
     ?_assertEqual(<<"3:2::bla">>, iolist_to_binary(message(2, "", "bla"))),
     ?_assertEqual(<<"3:::bla">>, iolist_to_binary(message("", "", "bla"))),
     ?_assertEqual(<<"3:4+:b:bla">>, iolist_to_binary(message("4+", "b", "bla"))),
     ?_assertEqual(<<"3::/test:bla">>, iolist_to_binary(message("", "/test", "bla")))].

json_test_() ->
    [?_assertEqual(<<"4:1::{\"a\":\"b\"}">>,
                  iolist_to_binary(json(1, "", [{<<"a">>,<<"b">>}]))),
     %% No demo for this, but the specs specify it
     ?_assertEqual(<<"4:1:/test:{\"a\":\"b\"}">>,
                  iolist_to_binary(json(1, "/test", [{<<"a">>,<<"b">>}]))),
     ?_assertEqual(<<"4:1+:/test:{\"a\":\"b\"}">>,
                  iolist_to_binary(json("1+", "/test", [{<<"a">>,<<"b">>}]))),
     ?_assertEqual(<<"4::/test:{\"a\":\"b\"}">>,
         iolist_to_binary(json("", "/test", [{<<"a">>,<<"b">>}])))].

event_test_() ->
    [
        ?_assertEqual(<<"5:1:/test:car", ?FRAME, "{\"a\":\"b\"}">>,
            iolist_to_binary(event(1,"/test","car",[{<<"a">>,<<"b">>}]))),
        ?_assertEqual(<<"5:::car", ?FRAME, "{\"a\":\"b\"}">>,
            iolist_to_binary(event("","",<<"car">>,[{<<"a">>,<<"b">>}]))),
        ?_assertEqual(<<"5:3+::car", ?FRAME, "{\"a\":\"b\"}">>,
            iolist_to_binary(event("3+","",<<"car">>,[{<<"a">>,<<"b">>}]))),
        ?_assertError(badarg, event("","","message",[])),
        ?_assertError(badarg, event("","",<<"message">>,[])),
        ?_assertError(badarg, event("","","connect",[])),
        ?_assertError(badarg, event("","",<<"connect">>,[])),
        ?_assertError(badarg, event("","","disconnect",[])),
        ?_assertError(badarg, event("","",<<"disconnect">>,[])),
        ?_assertError(badarg, event("","","open",[])),
        ?_assertError(badarg, event("","",<<"open">>,[])),
        ?_assertError(badarg, event("","","close",[])),
        ?_assertError(badarg, event("","",<<"close">>,[])),
        ?_assertError(badarg, event("","","error",[])),
        ?_assertError(badarg, event("","",<<"error">>,[])),
        ?_assertError(badarg, event("","","retry",[])),
        ?_assertError(badarg, event("","",<<"retry">>,[])),
        ?_assertError(badarg, event("","","reconnect",[])),
        ?_assertError(badarg, event("","",<<"reconnect">>,[]))
    ].

ack_test_() ->
    [?_assertEqual(<<"6:::4">>, iolist_to_binary(ack(4))),
     ?_assertEqual(<<"6:::4+[\"A\",\"B\"]">>,
         iolist_to_binary(ack(4,[<<"A">>,<<"B">>])))].

error_test_() ->
    %% No example, liberal interpretation
    [?_assertEqual(<<"7::end:you+die">>,
            iolist_to_binary(error("end","you","die"))),
        ?_assertEqual(<<"7:::you">>, iolist_to_binary(error("","you")))].

%% DECODING TESTS
d_disconnect_test_() ->
    [?_assertEqual({disconnect, <<"/test">>}, decode(<<"0::/test">>)),
     ?_assertEqual(disconnect, decode(<<"0">>))].

d_connect_test_() ->
    [].

%%% No format specified in the spec.
d_heartbeat_test() ->
    ?assertEqual(heartbeat, decode(heartbeat())).

d_message_test_() ->
    [?_assertEqual({message, 1, "", <<"blabla">>}, decode(iolist_to_binary(message(1, "", "blabla")))),
     ?_assertEqual({message, 2, "", <<"bla">>}, decode(iolist_to_binary(message(2, "", "bla")))),
     ?_assertEqual({message, "", "", <<"bla">>}, decode(iolist_to_binary(message("", "", "bla")))),
     ?_assertEqual({message, "4+", "b", <<"bla">>}, decode(iolist_to_binary(message("4+", "b", "bla")))),
     ?_assertEqual({message, "", "/test", <<"bla">>}, decode(iolist_to_binary(message("", "/test", "bla"))))].

d_json_test_() ->
    [?_assertEqual({json, 1, "", [{<<"a">>,<<"b">>}]},
            decode(iolist_to_binary(json(1, "", [{<<"a">>,<<"b">>}])))),
        ?_assertEqual({json, 1, "/test", [{<<"a">>,<<"b">>}]},
            decode(iolist_to_binary(json(1, "/test", [{<<"a">>,<<"b">>}])))),
        ?_assertEqual({json, "1+", "/test", [{<<"a">>,<<"b">>}]},
            decode(iolist_to_binary(json("1+", "/test", [{<<"a">>,<<"b">>}])))),
        ?_assertEqual({json, "", "/test", [{<<"a">>,<<"b">>}]},
            decode(iolist_to_binary(json("", "/test", [{<<"a">>,<<"b">>}]))))].

d_event_test_() ->
    [
        ?_assertEqual({event, 1, "/test", <<"car">>, [{<<"a">>,<<"b">>}]},
            decode(iolist_to_binary(event(1,"/test","car",[{<<"a">>,<<"b">>}])))),
        ?_assertEqual({event, "", "", <<"car">>, [{<<"a">>,<<"b">>}]},
            decode(iolist_to_binary(event("","",<<"car">>,[{<<"a">>,<<"b">>}])))),
        ?_assertEqual({event, "3+", "", <<"car">>, [{<<"a">>,<<"b">>}]},
            decode(iolist_to_binary(event("3+","",<<"car">>,[{<<"a">>,<<"b">>}]))))
    ].

d_ack_test_() ->
    [?_assertEqual({ack, 4}, decode(iolist_to_binary(ack(4)))),
    ?_assertEqual({ack, 4, [<<"A">>,<<"B">>]},
                  decode(iolist_to_binary(ack(4,[<<"A">>,<<"B">>]))))].

d_error_test_() ->
    %% No example, liberal interpretation
    [?_assertEqual({error, "end", <<"you">>, <<"die">>},
            decode(iolist_to_binary(error("end","you","die")))),
        ?_assertEqual({error, "", <<"you">>}, decode(iolist_to_binary(error("","you"))))].


%%% Guillermo has no idea whether we frame the length or the whole message.
%%% The spec means nothing there?
%decode(Data = [_|_]) ->
%    {Length, Msg} = len(Data, undefined),
%    {Packet, MoreData} = lists:split(Length, Msg),
%    [parse(Packet) | case MoreData of
%                        [] -> [];
%                        _ -> decode(MoreData)
%                    end].
%
%len([?FRAME, Num | Rest], undefined) when Num >= $0, Num =< $9 ->
%    len(Rest, Num-$0);
%len([?FRAME | Rest], Length) ->
%    {Length, Rest};
%len([Num | Rest], Length) when Num >= $0, Num =< $9 ->
%    len(Rest, Num-$0 + Length * 10).
