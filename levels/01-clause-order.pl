:- ensure_loaded('../tools/level').
:- name("The endless corridor").
:- concept("Clause order").
:- description("A door leads from the entrance to the treasure, but the adventurer gets lost before seeing it.").
:- query(room(treasure)).
:- torch(20).
:- tools([move_clauses(1)]).

room(X) :- room(Y), door(Y, X).
room(entrance).

door(entrance, treasure).
