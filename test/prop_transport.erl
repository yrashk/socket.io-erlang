-module(prop_transport).
-compile(export_all).
-include("../include/socketio.hrl").
-include_lib("proper/include/proper.hrl").

%% Sanity checks that make sure the encoder and decoders understand
%% each other.
%% These tests DO NOT prove that we're actually decoding or encoding
%% anything. Other properties need to account for that.
prop_sanity_msg() ->
    ?FORALL(Str, gen_string(),
     begin
        S = eval(Str),
        String = #msg{content=S},
        Encoded = socketio_data:encode(String),
        [#msg{content=Parsed}] = socketio_data:decode(#msg{content=Encoded}),
        ?WHENFAIL(
            io:format("~p =/= ~p~n", [S,Parsed]),
            S =:= Parsed
        )
     end).

prop_sanity_heartbeat() ->
    ?FORALL(N, heartbeat(),
      begin
        Hb = #heartbeat{index=N},
        Encoded = socketio_data:encode(Hb),
        [#heartbeat{index=Parsed}] = socketio_data:decode(#msg{content=Encoded}),
        ?WHENFAIL(
            io:format("~p =/= ~p~n", [N, Parsed]),
            N =:= Parsed
        )
      end).

prop_sanity_json() ->
    ?FORALL(Term, json(),
      begin
        J = #msg{content=Term, json=true},
        Encoded = socketio_data:encode(J),
        [#msg{content=Parsed, json=true}] = socketio_data:decode(#msg{content=Encoded}),
        ?WHENFAIL(
            io:format("~p =/= ~p~n", [Term, Parsed]),
            Term =:= Parsed
        )
      end).

%% Given the nature of the socket.io protocol and the environment it is part of,
%% it will happen that messages will be appended one to each other. The parser
%% Should thus return a list of messages to be considered, in the order they
%% were received (we assume first received is first, here)
prop_client_buffered_messages_count() ->
    ?FORALL({MsgNum, MsgString}, gen_encoded_many(),
      begin
        Decoded = socketio_data:decode(#msg{content=eval(MsgString)}),
        equals(length(Decoded), MsgNum)
      end).

prop_client_buffered_messages_sanity() ->
    ?FORALL({_, MsgString}, gen_encoded_many(),
      begin
        Decoded = socketio_data:decode(#msg{content=eval(MsgString)}),
        Encoded = lists:flatten([socketio_data:encode(M) || M <- Decoded]),
        ?WHENFAIL(io:format("~p =/= ~p~n",[MsgString, Encoded]),
        equals(MsgString, Encoded))
      end).

%%% GENERATORS
%% generates strings with a more certain presence of escape characters
gen_encoded_many() ->
    ?SUCHTHAT(X, gen_encoded({0, ""}), X =/= {0, ""}).

gen_encoded({N, Encoded}) ->
    ?LET(E, Encoded,
      ?LAZY(union([
          {N, Encoded},
          gen_encoded({N+1, ?LET(X, heartbeat(), E++socketio_data:encode(#heartbeat{index=X}))}),
          gen_encoded({N+1, ?LET(X, json(), E++socketio_data:encode(#msg{content=X, json=true}))}),
          gen_encoded({N+1, ?LET(X, gen_string(), E++socketio_data:encode(#msg{content=X}))})
      ]))
    ).

gen_string() ->
    ?LAZY(weighted_union([
        {1, []},
        {1, [$~|proper_stdgen:utf8_string()]},
        {10, [proper_stdgen:unicode_char()|gen_string()]}
    ])).

heartbeat() -> ?LET(N, int(), abs(N)).

json() ->
    ?LET(Compiled,
         ?LET(X, [json_data(),end_json], lists:flatten(X)),
         jsx:json_to_term(jsx:format(jsx:eventify(Compiled)))).

json_data() ->
    ?LAZY(weighted_union([
        {1,?LET(X, [start_array, json_data(), end_array], lists:flatten(X))},
        {1,?LET(X, [start_object,json_object(),end_object], lists:flatten(X))}
    ])).

json_object() ->
    ?LAZY(union([[], [key(), val()]])).

key() -> {key, proper_stdgen:utf8_string()}.
val() ->
    union([
        {literal,true},
        {literal,false},
        {literal,null},
        {integer,?LET(N, int(), integer_to_list(N))},
        {string, proper_stdgen:utf8_string()},
        {float,
         ?LET({A,B}, {int(),nat()}, integer_to_list(A)++[$.]++integer_to_list(B))}
    ]).

ascii() ->
    list(choose($a,$z)).
