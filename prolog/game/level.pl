:- module(level, [load_level/2]).

:- use_module('../engine/engine', [conj_list/2]).

load_level(File, Level) :-
    read_terms(File, Terms),
    split_terms(Terms, Directives, Clauses),
    level_info(Directives, Info),
    maplist(program_clause, Clauses, Program),
    Level = Info.put(program, Program).

read_terms(File, Terms) :-
    setup_call_cleanup(open(File, read, In),
                       read_all(In, Terms),
                       close(In)).

read_all(In, Terms) :-
    read_term(In, Term, [variable_names(Names)]),
    (   Term == end_of_file
    ->  Terms = []
    ;   Terms = [Term-Names|Rest],
        read_all(In, Rest)
    ).

split_terms([], [], []).
split_terms([(:- Directive)-Names|Terms], [Directive-Names|Directives], Clauses) :- !,
    split_terms(Terms, Directives, Clauses).
split_terms([Clause|Terms], Directives, [Clause|Clauses]) :-
    split_terms(Terms, Directives, Clauses).

level_info(Directives, Info) :-
    maplist(directive_fields, Directives, FieldLists),
    append(FieldLists, Fields),
    dict_pairs(Given, level, Fields),
    Defaults = level{concept: "", description: "", tools: []},
    Info = Defaults.put(Given),
    forall(member(Key, [name, query, torch]), required(Key, Info)).

directive_fields(ensure_loaded(_)-_,  []) :- !.
directive_fields(name(Name)-_,        [name-Name]) :- !.
directive_fields(concept(Text)-_,     [concept-Text]) :- !.
directive_fields(description(Text)-_, [description-Text]) :- !.
directive_fields(query(Query)-Names,  [query-Goals, query_vars-Names]) :- !,
    conj_list(Query, Goals).
directive_fields(torch(Steps)-_,      [torch-Steps]) :- !,
    must_be(positive_integer, Steps).
directive_fields(tools(Tools)-_,      [tools-Tools]) :- !,
    must_be(list, Tools).
directive_fields(Directive-_, _) :-
    domain_error(level_directive, Directive).

required(Key, Info) :-
    (   get_dict(Key, Info, _)
    ->  true
    ;   existence_error(level_directive, Key)
    ).

program_clause((Head :- Body)-Names, clause(Head, Goals, Names)) :- !,
    must_be(callable, Head),
    conj_list(Body, Goals).
program_clause(Fact-Names, clause(Fact, [], Names)) :-
    must_be(callable, Fact).
