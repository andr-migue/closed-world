:- module(node, [make_node/4]).

:- use_module(goals).

make_node(Program, Answer, Goals, node(Answer, Goals, Doors)) :-
    doors(Program, Goals, Doors).

doors(_, [], []).
doors(_, [Goal|_], []) :-
    builtin(Goal).
doors(Program, [Goal|_], Doors) :-
    \+ builtin(Goal),
    functor(Goal, Name, Arity),
    findall(door(Index, Clause),
            (   nth1(Index, Program, Clause),
                Clause = clause(Head, _, _),
                functor(Head, Name, Arity)
            ),
            Doors).
