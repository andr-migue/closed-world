:- module(events, [event_text/2, reason_text/2, result_text/2]).

:- use_module(terms).

event_text(enter(Door, Goal), Text) :-
    term_text(Goal, G),
    format(string(Text), "door ~w opens: ~s", [Door, G]).
event_text(mismatch(Door, Goal, Reason), Text) :-
    term_text(Goal, G),
    reason_text(Reason, R),
    format(string(Text), "door ~w stays shut for ~s (~s)", [Door, G, R]).
event_text(backtrack, "dead end, going back").
event_text(cut([]), "cut: no doors left to close").
event_text(cut(Doors), Text) :-
    Doors \== [],
    atomic_list_concat(Doors, ', ', List),
    format(string(Text), "cut: doors ~w close behind", [List]).
event_text(not_start(Goal), Text) :-
    term_text(Goal, G),
    format(string(Text), "a ghost explores ahead: not ~s", [G]).
event_text(not_fails, "the ghost found a way out: not fails").
event_text(not_succeeds, "the ghost found nothing: not succeeds").
event_text(trap, "TRAP!").
event_text(success(_), "the treasure is here").

reason_text(clash(A, B), Text) :-
    term_text(A, TA),
    term_text(B, TB),
    format(string(Text), "~s does not match ~s", [TA, TB]).
reason_text(cycle(Var, Term), Text) :-
    term_text(Var-Term, VT),
    format(string(Text), "occurs check: ~s", [VT]).

result_text(success([]), "TREASURE!").
result_text(success(Answer), Text) :-
    Answer \== [],
    maplist(binding_text, Answer, Bindings),
    atomic_list_concat(Bindings, ', ', A),
    format(string(Text), "TREASURE! ~w", [A]).
result_text(trap, "The adventurer fell into a trap.").
result_text(failure, "No way out: the adventurer gives up.").
result_text(torch_out, "The torch went out.").

binding_text(Name=Value, Text) :-
    term_text(Value, V),
    format(string(Text), "~w = ~s", [Name, V]).
