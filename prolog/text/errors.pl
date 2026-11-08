:- module(errors, [error_text/2]).

error_text(permission_error(use, tool, Tool), Text) :- !,
    format(string(Text), "You have no ~w left.", [Tool]).
error_text(domain_error(Kind, Value), Text) :- !,
    format(string(Text), "Invalid ~w: ~q", [Kind, Value]).
error_text(Error, Text) :-
    format(string(Text), "Error: ~q", [Error]).
