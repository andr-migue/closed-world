:- module(goals, [conj_list/2, builtin/1, negated/2, cut_marker/2, cut_goal/2]).

conj_list(Goal, [Goal]) :- var(Goal), !.
conj_list((A, B), Goals) :- !,
    conj_list(A, GoalsA),
    conj_list(B, GoalsB),
    append(GoalsA, GoalsB, Goals).
conj_list(true, []) :- !.
conj_list(Goal, [Goal]).

builtin(Goal) :- Goal == trap.
builtin(Goal) :- cut_goal(Goal, _).
builtin(Goal) :- negated(Goal, _).

negated(Goal, Inner) :- nonvar(Goal), Goal = not(Inner).
negated(Goal, Inner) :- nonvar(Goal), Goal = (\+ Inner).

cut_marker(Height, '$cut'(Height)).

cut_goal(Goal, Height) :- nonvar(Goal), Goal = '$cut'(Height).
