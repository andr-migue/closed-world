:- ensure_loaded('../tools/swi_runner').
:- name("The two wings").
:- concept("Scope of the cut").
:- description("Gold opens the west wing, but iron is cursed. You have a single cut.").
:- query((wing(W), take(M), use(M, W))).
:- torch(30).
:- tools([add_cut(1)]).

wing(east).
wing(west).

take(gold).
take(iron) :- trap.

use(gold, west).
