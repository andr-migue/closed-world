:- module(positions, [remove_at/5, insert_at/5, move/5]).

remove_at(Kind, Pos, List, Elem, Rest) :-
    length(List, Len),
    check_position(Kind, Pos, Len),
    nth1(Pos, List, Elem, Rest).

insert_at(Kind, Pos, Elem, List, Result) :-
    length(List, Len),
    Max is Len + 1,
    check_position(Kind, Pos, Max),
    nth1(Pos, Result, Elem, List).

move(Kind, From, To, List, Result) :-
    remove_at(Kind, From, List, Elem, Rest),
    length(List, Len),
    check_position(Kind, To, Len),
    (   From =:= To
    ->  domain_error(new_position, To)
    ;   nth1(To, Result, Elem, Rest)
    ).

check_position(Kind, Pos, Max) :-
    (   integer(Pos), between(1, Max, Pos)
    ->  true
    ;   domain_error(Kind, Pos)
    ).
