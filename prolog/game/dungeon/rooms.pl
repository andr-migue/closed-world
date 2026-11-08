:- module(rooms, [add_room/6, add_door/6, close_doors/5, mark/5, mark_ghost/3,
                  relabel/4, first_goal/2]).

:- use_module(library(assoc)).
:- use_module('../../engine/goals', [cut_goal/2]).

add_room(Parent, Goal, Step, State0, Id, State) :-
    Id = State0.next_id,
    get_assoc(Parent, State0.rooms, P),
    Row is P.row + 1,
    (   P.children =:= 0
    ->  Col = P.col,
        NextCol = State0.next_col
    ;   Col = State0.next_col,
        NextCol is Col + 1
    ),
    Children is P.children + 1,
    put_assoc(Parent, State0.rooms, P.put(children, Children), Rooms1),
    Room = room{id: Id, parent: Parent, row: Row, col: Col, goal: Goal,
                ghost: P.ghost, step: Step, kind: room, kind_step: Step, children: 0},
    put_assoc(Id, Rooms1, Room, Rooms),
    NextId is Id + 1,
    State = State0.put(_{rooms: Rooms, next_id: NextId, next_col: NextCol}).

add_door(Room, Clause, Status, Step, State0, State) :-
    Door = door{room: Room, clause: Clause, status: Status, step: Step},
    State = State0.put(doors, [Door|State0.doors]).

close_doors(Step, node(_, _, Doors), Room, State0, State) :- !,
    foldl([door(Clause, _), S0, S]>>add_door(Room, Clause, closed, Step, S0, S),
          Doors, State0, State).
close_doors(_, _, _, State, State).

mark(Id, Kind, Step, State0, State) :-
    update_room(Id, kind, Kind, State0, State1),
    update_room(Id, kind_step, Step, State1, State).

mark_ghost(Id, State0, State) :-
    update_room(Id, ghost, true, State0, State).

relabel(Id, Goals, State0, State) :-
    first_goal(Goals, Goal),
    update_room(Id, goal, Goal, State0, State).

update_room(Id, Key, Value, State0, State) :-
    get_assoc(Id, State0.rooms, Room),
    put_assoc(Id, State0.rooms, Room.put(Key, Value), Rooms),
    State = State0.put(rooms, Rooms).

first_goal([], none).
first_goal([Goal|_], !) :- cut_goal(Goal, _), !.
first_goal([Goal|_], Goal).
