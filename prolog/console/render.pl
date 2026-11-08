:- module(render, [show_help/1, show_levels/1, show_level/1, show_trace/1,
                   show_result/4, show_error/1]).

:- use_module('../text/terms').
:- use_module('../text/events').
:- use_module('../text/errors').

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
    format("Torch: ~w   Tools: ~s   Edits: ~w~n", [Level.torch, ToolsText, Edits]),
    show_par(Level.par).

show_par(none).
show_par(par(Torch, Edits)) :-
    format("Par: ~w torch, ~w edits~n", [Torch, Edits]).

show_trace(Trace) :-
    forall(nth1(I, Trace, Event),
           (   event_text(Event, Text),
               format("  [~w] ~s~n", [I, Text])
           )).

show_result(Result, TorchUsed, Stars, Level) :-
    result_text(Result, Text),
    stars_text(Stars, StarsText),
    edits(Level, Edits),
    format("~n~s~nTorch used: ~w / ~w   Edits: ~w   ~s~n",
           [Text, TorchUsed, Level.torch, Edits, StarsText]).

show_error(Error) :-
    error_text(Error, Text),
    format("~s~n", [Text]).

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
