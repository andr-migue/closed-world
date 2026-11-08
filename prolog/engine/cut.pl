:- module(cut, [mark_cuts/3, apply_cut/7]).

:- use_module(goals).
:- use_module(node).

mark_cuts(Height, Goals, Marked) :-
    maplist(mark_cut(Height), Goals, Marked).

mark_cut(Height, Goal, Marker) :- Goal == !, cut_marker(Height, Marker).
mark_cut(_, Goal, Goal) :- Goal \== !.

apply_cut(Program, Height, Answer, Goals, Stack, [Node|Kept], Closed) :-
    length(Kept, Height),
    append(Removed, Kept, Stack),
    closed_doors(Removed, Closed),
    make_node(Program, Answer, Goals, Node).

closed_doors(Removed, Closed) :-
    findall(Index,
            (   member(node(_, _, Doors), Removed),
                member(door(Index, _), Doors)
            ),
            Closed).
