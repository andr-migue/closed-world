<div align="center">

## Closed World

![SWI-Prolog](https://img.shields.io/badge/SWI--Prolog-10.0-E61B23?style=flat-square)
![Haskell](https://img.shields.io/badge/Haskell-GHC_9.6-5E5086?style=flat-square&logo=haskell&logoColor=white)
![Status](https://img.shields.io/badge/status-in_development-F5A623?style=flat-square)

</div>

Closed World is a step-by-step interpreter for a subset of Prolog, written in Prolog and presented as a puzzle game. Backtracking, the cut and negation as failure are represented explicitly, so every resolution step can be shown, counted and explained.

Every level is logically correct (the answer exists) but Prolog's search order keeps the adventurer from reaching it. You cannot rewrite the program, only change *how it runs*.

### Getting started

**Requirements:** [SWI-Prolog](https://www.swi-prolog.org/) 9 or later, and GHC 9.6 with the `brick` and `aeson` packages for the terminal interface.

```bash
git clone https://github.com/andr-migue/closed-world.git
cd closed-world
./closed-world
```

The first run builds the interface. Press a number to load a level and use the arrow keys to follow the adventurer step by step.

To play in plain text with Prolog only, run `./closed-world console`: type `levels` to list the levels, `level 1` to start, and `help` for the commands.

### Documentation

- [**Playing**](docs/playing.md): the dungeon, commands, levels and how to write your own
- [**Architecture**](docs/architecture.md): how the engine works and how the project is organized

### Roadmap

- [x] Step-by-step engine: SLD resolution, backtracking, cut, negation, trap and torch
- [x] Unification with occurs check
- [x] Level format and eight levels
- [x] Player edits with a limited tool budget
- [x] Playable console
- [x] Star rating
- [x] JSON server and dungeon layout
- [x] Terminal interface prototype in Haskell with brick
- [x] Program edits in the Haskell interface
- [ ] Half-block pixel art and fog memory in the Haskell interface
- [ ] Trace analyzer: SLD tree and metrics

> [!NOTE]
> Closed World is the final project for a Declarative Programming course. The name refers to the *closed world assumption*: in the dungeon, only what the program declares or can derive exists.
