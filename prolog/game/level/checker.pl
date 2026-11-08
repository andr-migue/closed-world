:- module(checker, [check_all/0, check_level/2]).

:- use_module(catalog).
:- use_module(reader).
:- use_module('../edit/edit').
:- use_module('../play').

check_all :-
    level_files(Files),
    maplist(report, Files).

report(File) :-
    load_level(File, Level),
    check_level(Level, Report),
    file_base_name(File, Name),
    format("~w  ~s~n", [Name, Level.name]),
    format("  unedited: ~w~n", [Report.unedited]),
    (   Report.solutions =:= 0
    ->  format("  NO SOLUTION with the allowed tools~n")
    ;   format("  solutions: ~w~n", [Report.solutions]),
        format("  best torch: ~w  ~q~n", [Report.best_torch, Report.best_torch_edits]),
        format("  fewest edits: ~w  ~q~n", [Report.fewest_edits, Report.fewest_edits_list]),
        par_check(Level.par, Report)
    ),
    nl.

par_check(none, _) :-
    format("  par: not declared~n").
par_check(par(T, E), Report) :-
    (   T =:= Report.best_torch, E =:= Report.fewest_edits
    ->  format("  par: ok~n")
    ;   format("  par: declared par(~w, ~w), best is par(~w, ~w)~n",
               [T, E, Report.best_torch, Report.fewest_edits])
    ).

check_level(Level, Report) :-
    play(Level, _, Unedited0, _),
    functor(Unedited0, Unedited, _),
    findall(Torch-Count-Edits,
            (   sequence(Level, Edited, Edits),
                Edits \== [],
                play(Edited, _, success(_), Torch),
                length(Edits, Count)
            ),
            Solutions),
    length(Solutions, N),
    Report0 = report{unedited: Unedited, solutions: N},
    (   N =:= 0
    ->  Report = Report0
    ;   aggregate_all(min(T), member(T-_-_, Solutions), BestTorch),
        aggregate_all(min(C), member(_-C-_, Solutions), FewestEdits),
        once(member(BestTorch-_-ByTorch, Solutions)),
        once(member(_-FewestEdits-ByEdits, Solutions)),
        Report = Report0.put(_{best_torch: BestTorch, best_torch_edits: ByTorch,
                               fewest_edits: FewestEdits, fewest_edits_list: ByEdits})
    ).

sequence(Level, Level, []).
sequence(Level0, Level, [Edit|Edits]) :-
    candidate(Level0, Edit),
    catch(apply_edit(Level0, Edit, Level1), error(_, _), fail),
    sequence(Level1, Level, Edits).

candidate(Level, move_clause(From, To)) :-
    length(Level.program, N),
    between(1, N, From),
    between(1, N, To),
    From =\= To.
candidate(Level, move_goal(Target, From, To)) :-
    target(Level, Target, K),
    between(1, K, From),
    between(1, K, To),
    From =\= To.
candidate(Level, add_cut(Target, Pos)) :-
    target(Level, Target, K),
    Max is K + 1,
    between(1, Max, Pos).
candidate(Level, remove_cut(Target, Pos)) :-
    target(Level, Target, K),
    between(1, K, Pos).

target(Level, query, K) :-
    length(Level.query, K).
target(Level, clause(I), K) :-
    nth1(I, Level.program, clause(_, Body, _)),
    length(Body, K).
