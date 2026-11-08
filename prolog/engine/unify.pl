:- module(unify, [unify/2, unify/3]).

unify(X, Y) :- var(X), var(Y), X = Y.
unify(X, Y) :- var(X), nonvar(Y), \+ occurs(X, Y), X = Y.
unify(X, Y) :- nonvar(X), var(Y), \+ occurs(Y, X), Y = X.
unify(X, Y) :- atomic(X), atomic(Y), X == Y.
unify(X, Y) :-
    compound(X), compound(Y),
    compound_name_arguments(X, Name, ArgsX),
    compound_name_arguments(Y, Name, ArgsY),
    maplist(unify, ArgsX, ArgsY).

unify(X, Y, Result) :-
    (   unify(X, Y)
    ->  Result = true
    ;   findall(Reason, conflict(X, Y, Reason), [Result|_])
    ).

occurs(Var, Term) :- Var == Term.
occurs(Var, Term) :-
    compound(Term),
    compound_name_arguments(Term, _, Args),
    member(Arg, Args),
    occurs(Var, Arg).

conflict(X, Y, cycle(X, Y)) :- var(X), X \== Y, occurs(X, Y).
conflict(X, Y, cycle(Y, X)) :- nonvar(X), var(Y), occurs(Y, X).
conflict(X, Y, Reason) :-
    compound(X), compound(Y),
    compound_name_arity(X, Name, Arity),
    compound_name_arity(Y, Name, Arity),
    compound_name_arguments(X, _, ArgsX),
    compound_name_arguments(Y, _, ArgsY),
    args_conflict(ArgsX, ArgsY, Reason).
conflict(X, Y, clash(X, Y)) :-
    nonvar(X), nonvar(Y),
    \+ same_functor(X, Y).

args_conflict([X|Xs], [Y|Ys], Reason) :-
    (   conflict(X, Y, Reason0)
    ->  Reason = Reason0
    ;   unify(X, Y),
        args_conflict(Xs, Ys, Reason)
    ).

same_functor(X, Y) :- atomic(X), atomic(Y), X == Y.
same_functor(X, Y) :-
    compound(X), compound(Y),
    compound_name_arity(X, Name, Arity),
    compound_name_arity(Y, Name, Arity).
