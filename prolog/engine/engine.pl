:- module(engine, []).

:- reexport(step, [initial_state/4, step/4]).
:- reexport(run, [run/6]).
:- reexport(goals, [conj_list/2]).
