:- module(resolve, [try_door/6]).

:- use_module(unify).
:- use_module(node).
:- use_module(cut).

try_door(Program, door(Index, Clause), Parent, Stack, Next, Event) :-
    Parent = node(Answer, Goals, _),
    copy_term(Answer-Goals-Clause, Answer1-[Goal|Rest]-clause(Head, Body, _)),
    unify(Goal, Head, Result),
    (   Result == true
    ->  length(Stack, Height),
        mark_cuts(Height, Body, Marked),
        append(Marked, Rest, Goals1),
        make_node(Program, Answer1, Goals1, Child),
        Next = [Child, Parent|Stack],
        Event = enter(Index, Goal)
    ;   Next = [Parent|Stack],
        Event = mismatch(Index, Goal, Result)
    ).
