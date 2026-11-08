:- ensure_loaded('../prolog/game/level/native_runner').
:- name("The two wings").
:- concept("Scope of the cut").
:- description("Gold opens the west wing, but iron is cursed. You have a single cut.").
:- hint("A cut commits to the clause it is in and to the choices made before it there. Where can it keep the adventurer away from iron without losing the west wing?").
:- query((wing(W), take(M), use(M, W))).
:- torch(30).
:- tools([add_cut(1)]).
:- par(9, 1).

wing(east).
wing(west).

take(gold).
take(iron) :- trap.

use(gold, west).
