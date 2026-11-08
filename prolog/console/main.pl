:- use_module('../game/level/catalog').
:- use_module('../game/level/reader').
:- use_module('../game/edit/edit').
:- use_module('../game/play').
:- use_module('../game/score').
:- use_module(commands).
:- use_module(render).

:- initialization(main, main).

main :-
    format("Closed World. Type 'help' for commands.~n"),
    loop(none).

loop(State) :-
    format("~n> "),
    flush_output,
    read_line_to_string(user_input, Line),
    (   Line == end_of_file
    ->  nl
    ;   parse_command(Line, Command),
        (   Command == quit
        ->  true
        ;   handle(Command, State, Next),
            loop(Next)
        )
    ).

handle(empty, State, State).
handle(help, State, State) :-
    command_help(Lines),
    show_help(Lines).
handle(levels, State, State) :-
    level_files(Files),
    findall(level(N, Name, Concept),
            (   nth1(N, Files, File),
                load_level(File, L),
                get_dict(name, L, Name),
                get_dict(concept, L, Concept)
            ),
            Levels),
    show_levels(Levels).
handle(level(N), State, Next) :-
    (   level_file(N, File)
    ->  load_level(File, Level),
        show_level(Level),
        Next = session(Level, Level)
    ;   format("There is no level ~w.~n", [N]),
        Next = State
    ).
handle(unknown(Line), State, State) :-
    format("Unknown command: ~s~n", [Line]).
handle(Command, none, none) :-
    needs_level(Command),
    format("Load a level first: level N~n").
handle(hint, session(Original, Current), session(Original, Current)) :-
    show_hint(Current).
handle(show, session(Original, Current), session(Original, Current)) :-
    show_level(Current).
handle(reset, session(Original, _), session(Original, Original)) :-
    show_level(Original).
handle(edit(Edit), session(Original, Current), session(Original, Next)) :-
    catch(( apply_edit(Current, Edit, Next),
            show_level(Next)
          ),
          error(Error, _),
          ( show_error(Error),
            Next = Current
          )).
handle(run, session(Original, Current), session(Original, Current)) :-
    play(Current, Trace, Result, TorchUsed),
    stars(Current, Result, TorchUsed, Stars),
    show_trace(Trace),
    show_result(Result, TorchUsed, Stars, Current).

needs_level(show).
needs_level(hint).
needs_level(reset).
needs_level(run).
needs_level(edit(_)).
