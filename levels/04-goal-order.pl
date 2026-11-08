:- ensure_loaded('../prolog/game/level/native_runner').
:- name("The key ring").
:- concept("Goal order").
:- description("Only one key opens the treasure door. Trying every key on every door burns out the torch.").
:- hint("Every goal multiplies the doors tried after it. Start with the goal that has the fewest answers.").
:- query((key(K), door(D), opens(K, D), leads_to_treasure(D))).
:- torch(25).
:- tools([move_goals(3)]).
:- par(16, 2).

key(bronze).
key(copper).
key(iron).
key(silver).
key(gold).

door(north).
door(south).
door(east).
door(west).
door(cellar).

opens(bronze, north).
opens(copper, south).
opens(iron, east).
opens(silver, west).
opens(gold, cellar).

leads_to_treasure(cellar).
