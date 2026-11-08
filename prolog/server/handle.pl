:- module(handle, [handle/2]).

:- use_module('../game/level/catalog').
:- use_module('../game/level/reader').
:- use_module('../game/edit/edit').
:- use_module('../game/play').
:- use_module('../game/dungeon/dungeon').
:- use_module(decode).
:- use_module(encode).

handle(Request, Reply) :-
    get_dict(op, Request, Op),
    handle(Op, Request, Reply).

handle("levels", _, _{levels: Levels}) :- !,
    level_files(Files),
    findall(Summary,
            (   nth1(Id, Files, File),
                load_level(File, Level),
                level_summary(Id, Level, Summary)
            ),
            Levels).
handle("level", Request, Reply) :- !,
    level_by_id(Request.id, Level),
    level_json(Request.id, Level, Reply).
handle("run", Request, Reply) :- !,
    level_by_id(Request.id, Level0),
    maplist(edit_from_json, Request.edits, Edits),
    apply_edits(Level0, Edits, Level),
    play(Level, Trace, Result, TorchUsed),
    length(Trace, Steps),
    dungeon(Level, Steps, Dungeon),
    run_json(Request.id, Level, Trace, Result, TorchUsed, Dungeon, Reply).
handle(Op, _, _) :-
    domain_error(op, Op).

level_by_id(Id, Level) :-
    (   level_file(Id, File)
    ->  load_level(File, Level)
    ;   existence_error(level, Id)
    ).
