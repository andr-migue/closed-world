:- ensure_loaded('../prolog/game/level/native_runner').
:- name("The locked study").
:- concept("Where the cut goes in a conjunction").
:- description("The adventurer commits to the first key on the ring before even trying the lock.").
:- hint("A cut removes the choices made before it in the same query. After it, key(K) cannot try another key.").
:- query((key(K), !, opens(K, study))).
:- torch(20).
:- tools([move_goals(1)]).
:- par(5, 1).

key(brass).
key(steel).
key(silver).

opens(silver, study).
