:- module(score, [stars/4]).

stars(_, Result, _, 0) :-
    Result \= success(_), !.
stars(Level, _, _, 3) :-
    get_dict(par, Level, none), !.
stars(Level, _, TorchUsed, Stars) :-
    get_dict(par, Level, par(TorchPar, EditsPar)),
    edits(Level, Edits),
    bonus(TorchUsed =< TorchPar, TorchBonus),
    bonus(Edits =< EditsPar, EditsBonus),
    Stars is 1 + TorchBonus + EditsBonus.

bonus(Condition, 1) :- call(Condition), !.
bonus(_, 0).

edits(Level, Edits) :-
    (   get_dict(edits, Level, Edits)
    ->  true
    ;   Edits = 0
    ).
