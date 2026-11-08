:- module(decode, [edit_from_json/2]).

edit_from_json(Json, Edit) :-
    get_dict(edit, Json, Name),
    edit(Name, Json, Edit).

edit("move_clause", J, move_clause(J.from, J.to)) :- !.
edit("move_goal", J, move_goal(Target, J.from, J.to)) :- !,
    target(J.target, Target).
edit("add_cut", J, add_cut(Target, J.pos)) :- !,
    target(J.target, Target).
edit("remove_cut", J, remove_cut(Target, J.pos)) :- !,
    target(J.target, Target).
edit(Name, _, _) :-
    domain_error(edit, Name).

target("q", query) :- !.
target(N, clause(N)) :- integer(N), !.
target(T, _) :-
    domain_error(edit_target, T).
