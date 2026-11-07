# Closed World

A puzzle game in Prolog. Each level is a logic program: the player edits it with a limited set of tools, and then an adventurer runs it using SLD resolution. If the adventurer reaches the treasure, the level is cleared.

Under the hood it is a step-by-step interpreter for a subset of Prolog, where backtracking, the cut and negation as failure are represented explicitly.
