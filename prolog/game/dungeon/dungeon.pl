:- module(dungeon, [dungeon/3]).

:- use_module(library(assoc)).
:- use_module('../../engine/engine').
:- use_module(explore).
:- use_module(rooms).

dungeon(Level, Steps, Dungeon) :-
    get_dict(program, Level, Program),
    get_dict(query, Level, Query),
    get_dict(query_vars, Level, Vars),
    initial_state(Program, Query, Vars, Stack),
    Stack = [node(_, Goals, _)],
    first_goal(Goals, Goal),
    list_to_assoc([0-room{id: 0, parent: none, row: 0, col: 0, goal: Goal,
                          ghost: false, step: 0, kind: start, kind_step: 0,
                          children: 0}], Rooms),
    State0 = state{program: Program, stack: Stack, ids: [0], rooms: Rooms,
                   doors: [], path: [], next_id: 1, next_col: 1},
    replay(1, Steps, State0, State),
    assoc_to_values(State.rooms, RoomList),
    maplist([R0, R]>>del_dict(children, R0, _, R), RoomList, PlainRooms),
    reverse(State.doors, Doors),
    reverse(State.path, Path),
    Dungeon = dungeon{rooms: PlainRooms, doors: Doors, path: Path}.

replay(Step, Steps, State, State) :-
    Step > Steps.
replay(Step, Steps, State0, State) :-
    Step =< Steps,
    step(State0.program, State0.stack, Stack, Event),
    apply_event(Event, Step, State0.stack, Stack, State0, State1),
    (   final(Event)
    ->  current_room(State0.ids, Room)
    ;   current_room(State1.ids, Room)
    ),
    State2 = State1.put(_{stack: Stack, path: [Room|State1.path]}),
    Next is Step + 1,
    replay(Next, Steps, State2, State).

current_room([Room|_], Room).
current_room([], none).

final(success(_)).
final(trap).
