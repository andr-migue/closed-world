:- module(target, [goals_at/3, set_goals_at/4]).

:- use_module(positions).

goals_at(Level, query, Goals) :-
    get_dict(query, Level, Goals).
goals_at(Level, clause(Index), Body) :-
    get_dict(program, Level, Program),
    remove_at(clause_index, Index, Program, clause(_, Body, _), _).
goals_at(_, Target, _) :-
    \+ valid_target(Target),
    domain_error(edit_target, Target).

set_goals_at(Level0, query, Goals, Level) :-
    Level = Level0.put(query, Goals).
set_goals_at(Level0, clause(Index), Body, Level) :-
    get_dict(program, Level0, Program0),
    remove_at(clause_index, Index, Program0, clause(Head, _, Names), Rest),
    insert_at(clause_index, Index, clause(Head, Body, Names), Rest, Program),
    Level = Level0.put(program, Program).

valid_target(query).
valid_target(clause(_)).
