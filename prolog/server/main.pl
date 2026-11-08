:- use_module(library(json)).
:- use_module('../text/errors').
:- use_module(handle).

:- initialization(main, main).

main :-
    set_stream(user_input, encoding(utf8)),
    set_stream(user_output, encoding(utf8)),
    loop.

loop :-
    read_line_to_string(user_input, Line),
    (   Line == end_of_file
    ->  true
    ;   respond(Line),
        loop
    ).

respond("") :- !.
respond(Line) :-
    catch(( atom_json_dict(Line, Request, []),
            handle(Request, Reply)
          ),
          Error,
          error_reply(Error, Reply)),
    json_write_dict(user_output, Reply, [width(0)]),
    nl(user_output),
    flush_output(user_output).

error_reply(error(Error, _), _{error: Text}) :- !,
    error_text(Error, Text).
error_reply(Error, _{error: Text}) :-
    error_text(Error, Text).
