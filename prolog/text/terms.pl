:- module(terms, [clause_text/2, clause_parts/3, goals_text/3, goal_texts/3, term_text/2]).

clause_text(clause(Head, Body, Names), Text) :-
    copy_term(Head-Body-Names, H-B-Ns),
    name_variables(H-B, Ns),
    write_options(Options),
    (   B == []
    ->  format(string(Text), "~W.", [H, Options])
    ;   goals_conj(B, Conj),
        format(string(Text), "~W :- ~W.", [H, Options, Conj, Options])
    ).

clause_parts(clause(Head, Body, Names), HeadText, GoalTexts) :-
    copy_term(Head-Body-Names, H-B-Ns),
    name_variables(H-B, Ns),
    named_text(H, HeadText),
    maplist(named_text, B, GoalTexts).

goal_texts(Goals, Names, Texts) :-
    copy_term(Goals-Names, Gs-Ns),
    name_variables(Gs, Ns),
    maplist(named_text, Gs, Texts).

named_text(Term, Text) :-
    write_options(Options),
    format(string(Text), "~W", [Term, Options]).

goals_text(Goals, Names, Text) :-
    copy_term(Goals-Names, Gs-Ns),
    name_variables(Gs, Ns),
    goals_conj(Gs, Conj),
    write_options(Options),
    format(string(Text), "~W", [Conj, Options]).

term_text(Term, Text) :-
    copy_term(Term, T),
    numbervars(T, 0, _),
    write_options(Options),
    format(string(Text), "~W", [T, Options]).

write_options([quoted(true), numbervars(true), spacing(next_argument)]).

name_variables(Term, Names) :-
    maplist([Name=Var]>>(Var = '$VAR'(Name)), Names),
    term_variables(Term, Rest),
    maplist(=('$VAR'('_')), Rest).

goals_conj([], true).
goals_conj([G], G) :- !.
goals_conj([G|Gs], (G, C)) :- goals_conj(Gs, C).
