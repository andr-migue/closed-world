:- ensure_loaded('../prolog/game/level/native_runner').
:- name("The endless corridor").
:- concept("Clause order").
:- description("A door leads from the entrance to the treasure, but the adventurer gets lost before seeing it.").
:- hint("Prolog tries clauses from top to bottom. Which clause lets room(Y) stop asking for yet another room?").
:- query(room(treasure)).
:- torch(20).
:- tools([move_clauses(1)]).
:- par(4, 1).

room(X) :- room(Y), door(Y, X).
room(entrance).

door(entrance, treasure).
