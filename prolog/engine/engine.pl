:- module(engine, [initial_state/4, step/4, run/6]).

:- use_module(unify).

initial_state(Program, Goals, Vars, [Root]) :-
    make_node(Program, Vars, Goals, Root).

step(_, [node(Answer, [], _)|Stack], Stack, success(Answer)).
step(_, [node(_, [Goal|_], _)|_], [], trap) :-
    Goal == trap.
step(_, [node(_, [Goal|_], [])|Stack], Stack, backtrack) :-
    \+ builtin(Goal).
step(Program, [node(Answer, Goals, [door(Index, Clause)|Doors])|Stack], Next, Event) :-
    Parent = node(Answer, Goals, Doors),
    copy_term(Answer-Goals-Clause, Answer1-[Goal|Rest]-clause(Head, Body, _)),
    unify(Goal, Head, Result),
    (   Result == true
    ->  append(Body, Rest, Goals1),
        make_node(Program, Answer1, Goals1, Child),
        Next = [Child, Parent|Stack],
        Event = enter(Index, Goal)
    ;   Next = [Parent|Stack],
        Event = mismatch(Index, Goal, Result)
    ).

make_node(Program, Answer, Goals, node(Answer, Goals, Doors)) :-
    doors(Program, Goals, Doors).

doors(_, [], []).
doors(_, [Goal|_], []) :-
    builtin(Goal).
doors(Program, [Goal|_], Doors) :-
    \+ builtin(Goal),
    functor(Goal, Name, Arity),
    findall(door(Index, Clause),
            (   nth1(Index, Program, Clause),
                Clause = clause(Head, _, _),
                functor(Head, Name, Arity)
            ),
            Doors).

builtin(Goal) :- Goal == trap.

run(Program, Goals, Vars, Torch, Trace, Result) :-
    initial_state(Program, Goals, Vars, Stack),
    run_stack(Program, Stack, Torch, Trace, Result).

run_stack(_, [], _, [], failure).
run_stack(Program, [Node|Nodes], Torch, Trace, Result) :-
    step(Program, [Node|Nodes], Stack, Event),
    continue(Event, Program, Stack, Torch, Trace, Result).

continue(success(Answer), _, _, _, [success(Answer)], success(Answer)).
continue(trap, _, _, _, [trap], trap).
continue(Event, _, _, 0, [], torch_out) :-
    \+ final(Event).
continue(Event, Program, Stack, Torch, [Event|Trace], Result) :-
    \+ final(Event),
    Torch > 0,
    Torch1 is Torch - 1,
    run_stack(Program, Stack, Torch1, Trace, Result).

final(success(_)).
final(trap).
