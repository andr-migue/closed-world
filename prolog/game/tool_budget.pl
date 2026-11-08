:- module(tool_budget, [spend_tool/3]).

spend_tool(Level0, Edit, Level) :-
    edit_tool(Edit, Tool),
    get_dict(tools, Level0, Tools0),
    (   select(Available, Tools0, Remaining, Tools),
        Available =.. [Tool, Uses],
        Uses > 0
    ->  Left is Uses - 1,
        Remaining =.. [Tool, Left],
        Level = Level0.put(tools, Tools)
    ;   permission_error(use, tool, Tool)
    ).

edit_tool(move_clause(_, _),    move_clauses).
edit_tool(move_goal(_, _, _),   move_goals).
edit_tool(add_cut(_, _),        add_cut).
edit_tool(remove_cut(_, _),     remove_cut).
edit_tool(Edit, _) :-
    \+ known_edit(Edit),
    domain_error(edit, Edit).

known_edit(move_clause(_, _)).
known_edit(move_goal(_, _, _)).
known_edit(add_cut(_, _)).
known_edit(remove_cut(_, _)).
