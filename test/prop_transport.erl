-module(prop_transport).
-include("../include/socketio.hrl").
-include_lib("proper/include/proper.hrl").

prop_sanity_msg() ->
    ?FORALL(Str, gen_string(),
     begin
        S = eval(Str),
        String = #msg{content=S},
        Encoded = socketio_data:encode(String),
        #msg{content=Parsed} = socketio_data:decode(#msg{content=Encoded}),
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
        #heartbeat{index=Parsed} = socketio_data:decode(#msg{content=Encoded}),
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
        #msg{content=Parsed, json=true} = socketio_data:decode(#msg{content=Encoded}),
        ?WHENFAIL(
            io:format("~p =/= ~p~n", [Term, Parsed]),
            Term =:= Parsed
        )
      end).

%%% GENERATORS
%% generates strings with a more certain presence of escape characters
gen_string() ->
    ?LAZY(weighted_union([
        {1, []},
        {1, [$~|string()]},
        {10, [char()|gen_string()]}
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

key() -> {key, ascii()}.
val() ->
    union([
        {literal,true},
        {literal,false},
        {literal,null},
        {integer,?LET(N, int(), integer_to_list(N))},
        {string, ascii()},
        {float,
         ?LET({A,B}, {int(),nat()}, integer_to_list(A)++[$.]++integer_to_list(B))}
    ]).

ascii() ->
    list(choose($a,$z)).
