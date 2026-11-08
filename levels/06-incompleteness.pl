:- ensure_loaded('../prolog/game/level/native_runner').
:- name("The echoing halls").
:- concept("Incompleteness of depth-first search").
:- description("Every corridor runs both ways, and the adventurer keeps turning back. A way to the cellar exists.").
:- hint("Depth-first search follows the first rule to the end. A rule that only turns back should not be tried first.").
:- query(path(hall, cellar)).
:- torch(30).
:- tools([move_clauses(1)]).
:- par(16, 1).

path(X, Y) :- path(Y, X).
path(X, Y) :- door(X, Z), path(Z, Y).
path(X, X).

door(hall, tower).
door(cellar, tower).
