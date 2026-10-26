[![Upgraded with Claude](https://img.shields.io/badge/Upgraded%20with-Claude-D97757?logo=anthropic&logoColor=white)](https://claude.com/claude-code)
[![License: Apache 2.0](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE)
![Scala 3](https://img.shields.io/badge/Scala-3.10-DC322F?logo=scala&logoColor=white)

# Sudoku Solver

A Sudoku solver in Scala. It solves a puzzle the way a person would (eliminating
impossible digits, spotting digits that fit in only one place), narrates every step,
and guesses only when logic is not enough. You can also type hints to help it.

The original post about this project is on my blog:
<http://www.blog.andrea.lorenzani.name/2017/05/04/sudoku-solver-in-scala/>

> **Upgraded after 9 years.** The project was written in 2017 (Scala 2.12, sbt 0.13,
> ScalaTest 3.0). In 2026 it was upgraded with [Claude Code](https://claude.com/claude-code):
> dependencies, language, code, tests, docs and license were all refreshed. See
> [What changed](#what-changed-in-the-2026-upgrade).

## Requirements

* JDK 17 or newer (tested with 21)
* [sbt](https://www.scala-sbt.org/) 1.x (the build pins 1.12.15)

## Usage

A puzzle is a text file with 9 lines of 9 characters; `0` (or `.`, `*`, `_`) is an empty cell.
Some examples are in [`puzzles/`](puzzles).

```
sbt "run puzzles/medium-1.txt"        # solve and show every step
sbt "run -i puzzles/hard.txt"         # interactive: you can help when it gets stuck
sbt test                              # run the test suite
```

Exit code is 0 when solved, 1 for unreadable files, clashing digits or unsolvable puzzles,
2 when no file is given.

### Interactive mode

With `-i`, when the solver can no longer make progress by logic it asks you for hints
(coordinates are 0-based, `x` is the column and `y` the row). If the input ends, it falls back to guessing.

```
i x y      display the possible values of the cell (x, y)
+ x y val  assign val (1 to 9) to (x, y)
- x y val  remove the possibility val (1 to 9) from (x, y)
? or h     show the help
```

## How it works

1. **Naked singles** – when a cell has a single candidate left, that digit is removed from
   the other cells of its row, column and 3x3 box.
2. **Hidden singles** – if a digit fits in only one cell of a row, column or box, it goes there.
3. **Branching** – when stuck, the cell with the fewest candidates is picked and each of its
   digits is tried, depth first, until one leads to a solution.

Code map (`src/main/scala/name/lorenzani/andrea/sudoku`):

| File | Role |
|------|------|
| `Candidates.scala` | the possible digits of a cell, as a bit set (opaque type) |
| `Coord.scala` | cell coordinates, peers and the 27 units |
| `Board.scala` | the immutable grid |
| `Solver.scala` | propagation + search, returns `Either[SolveError, Solution]` |
| `Command.scala` | parser for the interactive commands |
| `SudokuReader.scala` | parser/reader for puzzle files |
| `Main.scala` | the `@main` entry point and rendering |

## What changed in the 2026 upgrade

### Toolchain and dependencies

| | 2017 | 2026 |
|---|---|---|
| Scala | 2.12.2 | 3.10.0 |
| sbt | 0.13.15 | 1.12.15 |
| Test library | ScalaTest 3.0.1 (`FlatSpec`) | MUnit 1.3.6 |
| Scalactic | 3.0.1 | removed (unused) |
| JDK | 8 | 17+ (built and tested on 21) |

### Code

* **Immutable model.** `Sudoku`, `SValue`, `FinalValue` and `OptionalValue` (mutable arrays, `var`s,
  in-place `remove`/`mutate`/`copy`) became a single immutable `Board` of `Candidates`.
  "Solved" is simply "one candidate left", so the final/optional class hierarchy is gone.
* **Scala 3 idioms:** `enum`s (`Event`, `SolveError`, `Command`), `opaque type`, `extension` methods,
  optional braces, `@main` instead of `extends App`, top-level definitions, `Using` for resources.
* **Errors as values.** Parsing and solving return `Either` instead of throwing from `require`
  and `IllegalStateException`. The CLI turns them into messages and exit codes.
* **No more Futures.** The 2017 version forked a `Future` per candidate with `Await.result(..., Duration.Inf)`,
  without cancelling the losers. Branching is now a deterministic, lazy depth-first search, so results
  are reproducible and nothing is left running.
* **Separation of concerns.** The solver no longer prints or reads `System.in`; it returns a list of
  `Event`s that `Main` renders, and the interactive mode is an injected `Advisor` function.
* **Bugs fixed on the way:** the file reader leaked its file handle; the keyboard parser rejected the
  digit 9 (`< 9` instead of `<= 9`) and could crash on short commands; invalid input was silently
  swallowed in a loop.
* **Behaviour changes to be aware of:** the "empty" puzzle (formerly `sudokuimpossible.test`) is not
  impossible, it has many solutions, so it is now `puzzles/empty.txt` and gets solved. A puzzle with
  clashing digits or no solution now ends with an error message and exit code 1. Step output uses `\n`
  instead of `\n\r`.

### Tests

From 7 tests on two classes of the model to 43 tests in five suites, covering candidates, coordinates and
units, board invariants, the file/line parser, the command parser, and the solver (all sample puzzles,
a known solution, invalid and unsolvable inputs, branching, the advisor, event rendering).
The sample puzzles now live in `puzzles/` and are used by the tests.

### Docs and license

* README rewritten (it was a single link). Files renamed: `README` → `README.md`, and the puzzle files
  `*.test` → `puzzles/*.txt`.
* The license stays **Apache 2.0**: it is permissive, still current and fine for derivative work. The
  copyright years were extended to 2017-2026, the long per-file headers were replaced by short
  [SPDX](https://spdx.dev) identifiers, and a [`NOTICE`](NOTICE) file records the modifications
  as the license asks.

## License

[Apache License 2.0](LICENSE) – © 2017-2026 Andrea Lorenzani.
