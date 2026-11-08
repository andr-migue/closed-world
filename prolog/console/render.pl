:- module(render, [show_help/1, show_levels/1, show_level/1, show_trace/1,
                   show_result/3, show_error/1]).

:- use_module('../text/terms').
:- use_module('../text/events').

show_help(Lines) :-
    forall(member(Line, Lines), format("  ~w~n", [Line])).

show_levels(Levels) :-
    forall(member(level(N, Name, Concept), Levels),
           format("  ~w  ~s  (~s)~n", [N, Name, Concept])).

show_level(Level) :-
    format("~n== ~s ==  (~s)~n", [Level.name, Level.concept]),
    format("~s~n~n", [Level.description]),
    forall(nth1(I, Level.program, Clause),
           (   clause_text(Clause, Text),
               format("  ~w  ~s~n", [I, Text])
           )),
    goals_text(Level.query, Level.query_vars, QueryText),
    format("~n  ?- ~s.~n~n", [QueryText]),
    tools_text(Level.tools, ToolsText),
    edits(Level, Edits),
    format("Torch: ~w   Tools: ~s   Edits: ~w~n", [Level.torch, ToolsText, Edits]).

show_trace(Trace) :-
    forall(nth1(I, Trace, Event),
           (   event_text(Event, Text),
               format("  [~w] ~s~n", [I, Text])
           )).

show_result(Result, TorchUsed, Level) :-
    result_text(Result, Text),
    edits(Level, Edits),
    format("~n~s~nTorch used: ~w / ~w   Edits: ~w~n", [Text, TorchUsed, Level.torch, Edits]).

show_error(permission_error(use, tool, Tool)) :- !,
    format("You have no ~w left.~n", [Tool]).
show_error(domain_error(Kind, Value)) :- !,
    format("Invalid ~w: ~q~n", [Kind, Value]).
show_error(Error) :-
    format("Error: ~q~n", [Error]).

tools_text([], "none").
tools_text(Tools, Text) :-
    Tools \== [],
    maplist(tool_text, Tools, Parts),
    atomic_list_concat(Parts, ', ', Text).

tool_text(Tool, Text) :-
    Tool =.. [Name, Uses],
    format(string(Text), "~w ×~w", [Name, Uses]).

edits(Level, Edits) :-
    (   get_dict(edits, Level, Edits)
    ->  true
    ;   Edits = 0
    ).
