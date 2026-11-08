:- module(step, [initial_state/4, step/4]).

:- use_module(goals).
:- use_module(node).
:- use_module(cut).
:- use_module(negation).
:- use_module(resolve).

initial_state(Program, Goals, Vars, [Root]) :-
    mark_cuts(0, Goals, Marked),
    make_node(Program, Vars, Marked, Root).

step(_, [node(Answer, [], _)|Stack], Stack, success(Answer)) :-
    \+ ghost_answer(Answer).
step(_, [node(Answer, [], _)|Stack], Next, not_fails) :-
    ghost_answer(Answer),
    negation_fails(Answer, Stack, Next).
step(Program, [negation(Answer, Goals)|Stack], Next, not_succeeds) :-
    negation_succeeds(Program, Answer, Goals, Stack, Next).
step(Program, [node(Answer, [Goal|Goals], _)|Stack], Next, not_start(Inner)) :-
    negated(Goal, Inner),
    start_negation(Program, Answer, Inner, Goals, Stack, Next).
step(_, [node(_, [Goal|_], _)|_], [], trap) :-
    Goal == trap.
step(Program, [node(Answer, [Goal|Goals], _)|Stack], Next, cut(Closed)) :-
    cut_goal(Goal, Height),
    apply_cut(Program, Height, Answer, Goals, Stack, Next, Closed).
step(_, [node(_, [Goal|_], [])|Stack], Stack, backtrack) :-
    \+ builtin(Goal).
step(Program, [node(Answer, Goals, [Door|Doors])|Stack], Next, Event) :-
    try_door(Program, Door, node(Answer, Goals, Doors), Stack, Next, Event).
