% ?- run.

:- dynamic level_data/1.

name(X)        :- assertz(level_data(name(X))).
concept(X)     :- assertz(level_data(concept(X))).
description(X) :- assertz(level_data(description(X))).
query(X)       :- assertz(level_data(query(X))).
torch(X)       :- assertz(level_data(torch(X))).
tools(X)       :- assertz(level_data(tools(X))).
par(T, E)      :- assertz(level_data(par(T, E))).

trap :- throw(trap).

run :-
    level_data(query(Q)),
    format("?- ~q.~n", [Q]),
    catch(call_with_inference_limit(Q, 100000, R), E, true),
    (   nonvar(E), E == trap          -> writeln("TRAP: the adventurer dies")
    ;   nonvar(E)                     -> format("ERROR: ~q~n", [E])
    ;   R == inference_limit_exceeded -> writeln("TORCH OUT: lost forever")
    ;   format("TREASURE: ~q~n", [Q])
    ),
    !.
run :-
    writeln("FAILURE: no way out").
