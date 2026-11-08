:- module(encode, [level_summary/3, level_json/3, run_json/7]).

:- use_module('../text/terms').
:- use_module('../text/events').

level_summary(Id, Level, _{id: Id, name: Level.name, concept: Level.concept}).

level_json(Id, Level, Json) :-
    maplist(clause_text, Level.program, Program),
    maplist(clause_json, Level.program, Clauses),
    goals_text(Level.query, Level.query_vars, Query),
    goal_texts(Level.query, Level.query_vars, QueryGoals),
    maplist(tool_json, Level.tools, Tools),
    edits(Level, Edits),
    Json = _{id: Id, name: Level.name, concept: Level.concept,
             description: Level.description, program: Program, clauses: Clauses,
             query: Query, query_goals: QueryGoals,
             torch: Level.torch, tools: Tools, edits: Edits}.

clause_json(Clause, _{head: Head, goals: Goals}) :-
    clause_parts(Clause, Head, Goals).

run_json(Id, Level, Trace, Result, TorchUsed, Dungeon, Json) :-
    level_json(Id, Level, LevelJson),
    result_json(Result, ResultName, Answer),
    result_text(Result, Message),
    maplist(step_json, Trace, Dungeon.path, Steps),
    maplist(room_json, Dungeon.rooms, Rooms),
    maplist(door_json, Dungeon.doors, Doors),
    Json = _{level: LevelJson, result: ResultName, answer: Answer, message: Message,
             torch_used: TorchUsed, steps: Steps, rooms: Rooms, doors: Doors}.

result_json(success(Answer), success, Bindings) :- !,
    maplist(binding_json, Answer, Bindings).
result_json(Result, Result, []).

binding_json(Name=Value, _{name: Name, value: Text}) :-
    term_text(Value, Text).

step_json(Event, Room0, _{event: Name, clause: Clause, text: Text, room: Room}) :-
    none_null(Room0, Room),
    functor(Event, Name, _),
    event_clause(Event, Clause),
    event_text(Event, Text).

event_clause(enter(Clause, _), Clause) :- !.
event_clause(mismatch(Clause, _, _), Clause) :- !.
event_clause(_, null).

room_json(Room, _{id: Room.id, parent: Parent, row: Room.row, col: Room.col,
                  label: Label, kind: Room.kind, kind_step: Room.kind_step,
                  ghost: Room.ghost, step: Room.step}) :-
    none_null(Room.parent, Parent),
    goal_label(Room.goal, Label).

goal_label(none, "") :- !.
goal_label(Goal, Label) :-
    term_text(Goal, Label).

door_json(Door, Json) :-
    door_status(Door.status, Status, Target, Reason),
    none_null(Door.clause, Clause),
    Json = _{room: Door.room, clause: Clause, status: Status, target: Target,
             reason: Reason, step: Door.step}.

door_status(open(Room), open, Room, "") :- !.
door_status(ghost(Room), ghost, Room, "") :- !.
door_status(shut(Reason), shut, null, Text) :- !,
    reason_text(Reason, Text).
door_status(closed, closed, null, "").

tool_json(Tool, _{tool: Name, uses: Uses}) :-
    Tool =.. [Name, Uses].

none_null(none, null) :- !.
none_null(Value, Value).

edits(Level, Edits) :-
    (   get_dict(edits, Level, Edits)
    ->  true
    ;   Edits = 0
    ).
