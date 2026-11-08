:- module(commands, [parse_command/2, command_help/1]).

command_help([
    "levels               list the levels",
    "level N              load level N",
    "show                 show the current program",
    "hint                 show a hint for the level",
    "move C1 C2           move clause C1 to position C2",
    "goal T F To          move goal F to position To in T (T = clause number or q)",
    "cut T P              insert a cut at position P of T",
    "uncut T P            remove the cut at position P of T",
    "run                  release the adventurer",
    "reset                undo all edits",
    "quit                 leave"
]).

parse_command(Line, Command) :-
    split_string(Line, " \t", " \t", Parts0),
    exclude(==(""), Parts0, Parts),
    (   command(Parts, Command0)
    ->  Command = Command0
    ;   Command = unknown(Line)
    ).

command([], empty).
command(["help"], help).
command(["levels"], levels).
command(["level", N], level(Number)) :- int(N, Number).
command(["show"], show).
command(["hint"], hint).
command(["run"], run).
command(["reset"], reset).
command(["quit"], quit).
command(["move", A, B], edit(move_clause(From, To))) :-
    int(A, From), int(B, To).
command(["goal", T, A, B], edit(move_goal(Target, From, To))) :-
    target(T, Target), int(A, From), int(B, To).
command(["cut", T, P], edit(add_cut(Target, Pos))) :-
    target(T, Target), int(P, Pos).
command(["uncut", T, P], edit(remove_cut(Target, Pos))) :-
    target(T, Target), int(P, Pos).

target("q", query).
target("query", query).
target(S, clause(N)) :- int(S, N).

int(String, Int) :-
    catch(number_string(Int, String), _, fail),
    integer(Int).
