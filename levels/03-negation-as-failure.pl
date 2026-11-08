:- ensure_loaded('../prolog/game/level/native_runner').
:- name("The impatient ghost").
:- concept("Negation as failure with an unbound variable").
:- description("The ghost looks for a room without a monster to hide the treasure, but gives up before starting.").
:- query((free(R), treasure(R))).
:- torch(30).
:- tools([move_goals(1)]).
:- par(10, 1).

free(X) :- not(monster(X)), room(X).

room(crypt).
room(ossuary).
room(chapel).

monster(crypt).
monster(ossuary).

treasure(chapel).
