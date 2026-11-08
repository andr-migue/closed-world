:- module(run, [run/6]).

:- use_module(step).

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
