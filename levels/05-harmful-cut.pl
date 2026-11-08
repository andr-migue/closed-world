:- ensure_loaded('../prolog/game/level/native_runner').
:- name("The first door").
:- concept("A cut that removes solutions").
:- description("The adventurer always walks through the first door in sight and never looks back.").
:- hint("Once next/2 has picked a door, its cut forbids looking for another one, even if that door leads nowhere.").
:- query(path(entrance, treasure)).
:- torch(30).
:- tools([remove_cut(1)]).
:- par(21, 1).

path(X, X).
path(X, Z) :- next(X, Y), path(Y, Z).

next(X, Y) :- door(X, Y), !.

door(entrance, moat).
door(entrance, gallery).
door(gallery, treasure).
