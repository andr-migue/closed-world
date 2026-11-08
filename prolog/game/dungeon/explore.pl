:- module(explore, [apply_event/6]).

:- use_module(library(assoc)).
:- use_module(rooms).

apply_event(enter(Clause, _), Step, _, [node(_, Goals, _)|_], State0, State) :-
    State0.ids = [Top|_],
    first_goal(Goals, Goal),
    add_room(Top, Goal, Step, State0, Room, State1),
    add_door(Top, Clause, open(Room), Step, State1, State2),
    State = State2.put(ids, [Room|State0.ids]).
apply_event(mismatch(Clause, _, Reason), Step, _, _, State0, State) :-
    State0.ids = [Top|_],
    add_door(Top, Clause, shut(Reason), Step, State0, State).
apply_event(backtrack, Step, _, _, State0, State) :-
    State0.ids = [Top|Below],
    get_assoc(Top, State0.rooms, Room),
    (   Room.children =:= 0
    ->  mark(Top, dead_end, Step, State0, State1)
    ;   State1 = State0
    ),
    State = State1.put(ids, Below).
apply_event(success(_), Step, _, _, State0, State) :-
    State0.ids = [Top|Below],
    mark(Top, treasure, Step, State0, State1),
    State = State1.put(ids, Below).
apply_event(trap, Step, _, _, State0, State) :-
    State0.ids = [Top|_],
    mark(Top, trap, Step, State0, State1),
    State = State1.put(ids, []).
apply_event(cut(_), Step, [_|Below0], [node(_, Goals, _)|Kept], State0, State) :-
    State0.ids = [Top|BelowIds],
    length(Kept, Height),
    append(Removed, Kept, Below0),
    length(KeptIds, Height),
    append(RemovedIds, KeptIds, BelowIds),
    foldl(close_doors(Step), Removed, RemovedIds, State0, State1),
    relabel(Top, Goals, State1, State2),
    State = State2.put(ids, [Top|KeptIds]).
apply_event(not_start(_), Step, _, [node(_, Goals, _)|_], State0, State) :-
    State0.ids = [Top|_],
    first_goal(Goals, Goal),
    add_room(Top, Goal, Step, State0, Ghost, State1),
    mark_ghost(Ghost, State1, State2),
    add_door(Top, none, ghost(Ghost), Step, State2, State3),
    State = State3.put(ids, [Ghost|State0.ids]).
apply_event(not_fails, Step, _, Kept, State0, State) :-
    State0.ids = [Top|_],
    mark(Top, exit, Step, State0, State1),
    length(Kept, Height),
    length(KeptIds, Height),
    append(_, KeptIds, State0.ids),
    State = State1.put(ids, KeptIds).
apply_event(not_succeeds, _, _, [node(_, Goals, _)|_], State0, State) :-
    State0.ids = [Top|_],
    relabel(Top, Goals, State0, State).
