# Playing Closed World

Every level is a Prolog program that is **logically correct**: the answer exists. But Prolog's search order keeps the adventurer from reaching it — an endless left recursion, a trap behind the wrong door, a torch that burns out. You cannot rewrite the program. You can only change *how it runs*, with a small set of tools.

## The dungeon

The dungeon is the SLD tree of the level's query.

| Prolog | In the dungeon |
| :--- | :--- |
| Current goal | The room the adventurer stands in |
| Matching clause | A door that opens |
| Failure | A dead end |
| Backtracking | Walking back to the last untried door |
| Cut `!` | A door that shuts behind you |
| `not/1` | A ghost that explores ahead |
| Infinite loop | An endless corridor |
| Resolution step | One unit of torch |

The adventurer wins when the query succeeds, and loses when every path fails, when they step on `trap`, or when the torch runs out.

Reaching the treasure earns one star, plus one for not using more torch than the level's par and one for not using more edits. Matching the best known solution earns all three.

> [!NOTE]
> A torch that runs out does not prove that a program never terminates — only that it took more steps than the level allows.

## Terminal interface

Start it with `./closed-world`. It opens a menu with **Play**, **How to play** and **Quit**. Play lists the levels with the best stars earned in the session. Inside a level, the adventurer walks the dungeon map step by step, lit by a torch that shrinks as it burns; rooms left behind stay on the map, cold and faded, and a minimap shows the whole tree. Every panel scrolls when the program or the query does not fit. When the run ends, a dialog shows the result and what you can do next.

| Key | Watching a run |
| :--- | :--- |
| `Space` | Release the adventurer, pause or resume |
| `←` `→` | Previous / next step |
| `Home` `End` | First / last step |
| `+` `-` | Walk faster / slower |
| `W` `A` `S` `D` | Pan the map (stepping follows the adventurer again) |
| `m` | Show or hide the minimap |
| `PgUp` `PgDn` `[` `]` | Scroll the program; the mouse wheel scrolls any panel (with `Shift`, sideways) |
| `e` | Edit the program |
| `h` | Show or hide the level's hint |
| `?` | How to play |
| `n` | Next level, after reaching the treasure |
| `Esc` | Back to the levels |

| Key | Edit mode |
| :--- | :--- |
| Arrows | Select a clause head, a goal, or the end of a body |
| `Space` | Pick the selection, then drop it at the new position |
| `c` | Insert a cut at the selection |
| `x` | Remove the selected cut |
| `u` / `R` | Undo the last edit / undo all edits |
| `PgUp` `PgDn` `[` `]` | Scroll the program (the selection is always kept in view) |
| `h` | Show or hide the level's hint |
| `Enter` | Release the adventurer with the edited program |
| `Esc` | Cancel a pick, or stop editing |

A head moves the whole clause; a goal moves within its own clause or query.

## Text console

Start it with `./closed-world console`. It only needs SWI-Prolog.

| Command | Effect |
| :--- | :--- |
| `levels` | List the levels |
| `level N` | Load level `N` |
| `show` | Show the current program |
| `hint` | Show a hint for the level |
| `move C1 C2` | Move clause `C1` to position `C2` |
| `goal T F To` | Move goal `F` to position `To` in `T` |
| `cut T P` | Insert a cut at position `P` of `T` |
| `uncut T P` | Remove the cut at position `P` of `T` |
| `run` | Release the adventurer |
| `reset` | Undo all edits |
| `help` | List the commands |
| `quit` | Leave |

`T` is a clause number, or `q` for the query. Goal positions count from 1 within a clause body or the query. Each level only allows certain tools, a limited number of times.

## Levels

| # | Level | Concept | Tools |
| :---: | :--- | :--- | :--- |
| 1 | The endless corridor | Clause order | Move one clause |
| 2 | The two wings | Scope of the cut | One cut |
| 3 | The impatient ghost | Negation as failure with an unbound variable | Move one goal |
| 4 | The key ring | Goal order | Move three goals |
| 5 | The first door | A cut that removes solutions | Remove one cut |
| 6 | The echoing halls | Incompleteness of depth-first search | Move one clause |
| 7 | The locked study | Where the cut goes in a conjunction | Move one goal |
| 8 | The cursed armory | Clause order and the scope of the cut | Move one clause, one cut |

<details>
<summary><b>Solutions</b></summary>

<br/>

| # | Solution | Why it works |
| :---: | :--- | :--- |
| 1 | `move 2 1` | The base case is tried before the left-recursive rule |
| 2 | `cut 3 1` | The cut discards `iron` but keeps the `west` wing |
| 3 | `goal q 1 2` | `treasure(R)` binds `R` before `not` checks it (the cheapest; `goal 1 2 1` also works) |
| 4 | `goal q 1 3`, `goal q 4 1` | Filtering first fits within the torch |
| 5 | `uncut 3 2` | Without the cut, the adventurer can try the gallery |
| 6 | `move 1 3` | The symmetric rule goes last, so the doors are tried before turning back |
| 7 | `goal q 1 3` | A cut first removes nothing, and checking the lock first saves steps (`goal q 2 3`, cutting after the lock, also works) |
| 8 | `move 3 4`, `cut 3 1` | The sword comes before the axe, and the cut keeps the adventurer away from the axe while still trying the south hall |

</details>

## Writing a level

A level is a plain Prolog file in [`levels/`](../levels). Its metadata goes in directives, so the same file can be read as data by the engine and run by hand in SWI-Prolog:

```prolog
:- ensure_loaded('../prolog/game/level/native_runner').
:- name("The two wings").
:- concept("Scope of the cut").
:- description("Gold opens the west wing, but iron is cursed. You have a single cut.").
:- hint("A cut commits to the clause it is in and to the choices made before it there. Where can it keep the adventurer away from iron without losing the west wing?").
:- query((wing(W), take(M), use(M, W))).
:- torch(30).
:- tools([add_cut(1)]).
:- par(9, 1).

wing(east).
wing(west).
take(gold).
take(iron) :- trap.
use(gold, west).
```

| Directive | Meaning |
| :--- | :--- |
| `name/1` | Title shown to the player |
| `concept/1` | What the level teaches |
| `description/1` | The player's brief |
| `hint/1` | A hint that explains the concept without giving the solution (optional) |
| `query/1` | The fixed query the adventurer runs |
| `torch/1` | Steps available |
| `tools/1` | Allowed edits: `move_clauses(N)`, `move_goals(N)`, `add_cut(N)`, `remove_cut(N)` |
| `par/2` | Torch and edits of the best known solution, used for the stars |

`name/1`, `query/1` and `torch/1` are required. `trap/0` is a game predicate that kills the adventurer on the spot.

Levels are numbered by file name. To try one with plain Prolog semantics, run `swipl levels/02-cut-scope.pl` and then `?- run.`

To check a level, run `./closed-world check`. For every level it tries all the edits its tools allow and reports whether it has a solution, the best torch, the fewest edits and whether the declared par matches.
