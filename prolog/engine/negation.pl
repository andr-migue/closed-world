:- module(negation, [ghost_answer/1, start_negation/6, negation_fails/3, negation_succeeds/5]).

:- use_module(goals).
:- use_module(node).
:- use_module(cut).

ghost_answer(Answer) :- nonvar(Answer), Answer = ghost(_).

start_negation(Program, Answer, Inner, Goals, Stack, [Ghost, negation(Answer, Goals)|Stack]) :-
    length(Stack, Height),
    conj_list(Inner, InnerGoals),
    GhostHeight is Height + 1,
    mark_cuts(GhostHeight, InnerGoals, Marked),
    make_node(Program, ghost(Height), Marked, Ghost).

negation_fails(ghost(Height), Stack, Kept) :-
    length(Kept, Height),
    append(_, Kept, Stack).

negation_succeeds(Program, Answer, Goals, Stack, [Node|Stack]) :-
    make_node(Program, Answer, Goals, Node).
