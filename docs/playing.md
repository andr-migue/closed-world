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

> [!NOTE]
> A torch that runs out does not prove that a program never terminates — only that it took more steps than the level allows.

## A session

The terminal interface (`./closed-world`) replays a run step by step on the dungeon map. Editing the program is, for now, done in the text console (`./closed-world console`):

```text
> level 2

== The two wings ==  (Scope of the cut)
Gold opens the west wing, but iron is cursed. You have a single cut.

  1  wing(east).
  2  wing(west).
  3  take(gold).
  4  take(iron) :- trap.
  5  use(gold, west).

  ?- wing(W), take(M), use(M, W).

Torch: 30   Tools: add_cut ×1   Edits: 0

> run
  [1] door 1 opens: wing(east)
  [2] door 3 opens: take(gold)
  [3] door 5 stays shut for use(gold, east) (east does not match west)
  [4] dead end, going back
  [5] door 4 opens: take(iron)
  [6] TRAP!

The adventurer fell into a trap.

> cut 3 1
> run
  ...
TREASURE! W = west, M = gold
Torch used: 9 / 30   Edits: 1
```

## Commands

| Command | Effect |
| :--- | :--- |
| `levels` | List the levels |
| `level N` | Load level `N` |
| `show` | Show the current program |
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

<details>
<summary><b>Solutions</b></summary>

<br/>

| # | Solution | Why it works |
| :---: | :--- | :--- |
| 1 | `move 2 1` | The base case is tried before the left-recursive rule |
| 2 | `cut 3 1` | The cut discards `iron` but keeps the `west` wing |
| 3 | `goal 1 2 1` | `room(X)` binds `X` before `not` checks it |
| 4 | `goal q 4 1`, `goal q 4 2`, `goal q 4 3` | Filtering first fits within the torch |
| 5 | `uncut 3 2` | Without the cut, the adventurer can try the gallery |

</details>

## Writing a level

A level is a plain Prolog file in [`levels/`](../levels). Its metadata goes in directives, so the same file can be read as data by the engine and run by hand in SWI-Prolog:

```prolog
:- ensure_loaded('../prolog/game/level/native_runner').
:- name("The two wings").
:- concept("Scope of the cut").
:- description("Gold opens the west wing, but iron is cursed. You have a single cut.").
:- query((wing(W), take(M), use(M, W))).
:- torch(30).
:- tools([add_cut(1)]).

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
| `query/1` | The fixed query the adventurer runs |
| `torch/1` | Steps available |
| `tools/1` | Allowed edits: `move_clauses(N)`, `move_goals(N)`, `add_cut(N)`, `remove_cut(N)` |

`name/1`, `query/1` and `torch/1` are required. `trap/0` is a game predicate that kills the adventurer on the spot.

Levels are numbered by file name. To try one with plain Prolog semantics, run `swipl levels/02-cut-scope.pl` and then `?- run.`
