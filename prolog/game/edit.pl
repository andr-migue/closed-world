:- module(edit, [apply_edit/3, apply_edits/3]).

:- use_module(positions).
:- use_module(edit_target).
:- use_module(tool_budget).

apply_edits(Level, [], Level).
apply_edits(Level0, [Edit|Edits], Level) :-
    apply_edit(Level0, Edit, Level1),
    apply_edits(Level1, Edits, Level).

apply_edit(Level0, Edit, Level) :-
    spend_tool(Level0, Edit, Level1),
    transform(Edit, Level1, Level2),
    count_edit(Level2, Level).

transform(move_clause(From, To), Level0, Level) :-
    get_dict(program, Level0, Program0),
    move(clause_index, From, To, Program0, Program),
    Level = Level0.put(program, Program).
transform(move_goal(Target, From, To), Level0, Level) :-
    goals_at(Level0, Target, Goals0),
    move(goal_position, From, To, Goals0, Goals),
    set_goals_at(Level0, Target, Goals, Level).
transform(add_cut(Target, Pos), Level0, Level) :-
    goals_at(Level0, Target, Goals0),
    insert_at(goal_position, Pos, !, Goals0, Goals),
    set_goals_at(Level0, Target, Goals, Level).
transform(remove_cut(Target, Pos), Level0, Level) :-
    goals_at(Level0, Target, Goals0),
    remove_at(goal_position, Pos, Goals0, Removed, Goals),
    (   Removed == !
    ->  set_goals_at(Level0, Target, Goals, Level)
    ;   domain_error(cut_position, Pos)
    ).

count_edit(Level0, Level) :-
    (   get_dict(edits, Level0, Count0)
    ->  true
    ;   Count0 = 0
    ),
    Count is Count0 + 1,
    Level = Level0.put(edits, Count).
