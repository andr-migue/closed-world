:- ensure_loaded('../prolog/game/level/native_runner').
:- name("The cursed armory").
:- concept("Clause order and the scope of the cut").
:- description("The cursed axe hangs closest to the door. You can move one weapon and place one cut.").
:- hint("There are two problems: the axe is tried before the sword, and after a dead end the adventurer comes back for it.").
:- query((hall(H), grab(W), wield(W, H))).
:- torch(30).
:- tools([move_clauses(1), add_cut(1)]).
:- par(9, 2).

hall(north).
hall(south).

grab(axe) :- trap.
grab(sword).

wield(sword, south).
