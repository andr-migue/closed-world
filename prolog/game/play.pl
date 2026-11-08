:- module(play, [play/4]).

:- use_module('../engine/engine').

play(Level, Trace, Result, TorchUsed) :-
    get_dict(program, Level, Program),
    get_dict(query, Level, Query),
    get_dict(query_vars, Level, Vars),
    get_dict(torch, Level, Torch),
    run(Program, Query, Vars, Torch, Trace, Result),
    exclude(free_event, Trace, Paid),
    length(Paid, TorchUsed).

free_event(success(_)).
free_event(trap).
