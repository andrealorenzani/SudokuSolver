// SPDX-License-Identifier: Apache-2.0
// Copyright 2017-2026 Andrea Lorenzani

package name.lorenzani.andrea.sudoku

import java.nio.file.Path

class SolverSuite extends munit.FunSuite:

  private def load(name: String): Board = SudokuReader.read(Path.of("puzzles", name)).fold(fail(_), identity)

  private def assertSolutionFor(puzzle: Board, solution: Board): Unit =
    assert(solution.isSolved, s"not a valid solution:\n$solution")
    Coord.all.foreach: c =>
      puzzle(c).single.foreach(d => assertEquals(solution(c).single, Some(d), s"given at $c was changed"))

  List("easy-1.txt", "easy-2.txt", "medium-1.txt", "medium-2.txt", "hard.txt", "empty.txt").foreach: name =>
    test(s"solves $name"):
      val puzzle = load(name)
      val result = Solver.solve(puzzle).fold(e => fail(e.toString), identity)
      assertSolutionFor(puzzle, result.board)

  test("solves an easy puzzle to its known solution"):
    val expected = List(
      "537816294", "961247538", "284593761",
      "419358672", "376924815", "852671349",
      "693482157", "725169483", "148735926"
    )
    val result = Solver.solve(load("easy-1.txt")).toOption.get
    assertEquals(result.board.values.mkString, expected.mkString)

  test("a solved puzzle is returned untouched"):
    val solved = Solver.solve(load("easy-1.txt")).toOption.get.board
    val again  = Solver.solve(solved).toOption.get
    assertEquals(again.board, solved)
    assertEquals(again.events, Nil)

  test("reports clashing givens as invalid"):
    val board = Board.empty.assign(Coord(0, 0), 5).assign(Coord(1, 0), 5)
    assertEquals(Solver.solve(board).left.toOption, Some(SolveError.Invalid))

  test("reports consistent but unsolvable puzzles"):
    // cell (0,0) has no candidate left: its row holds 1-8 and the 9 sits in its column
    val values = List(0, 1, 2, 3, 4, 5, 6, 7, 8) ++ List(9) ++ List.fill(71)(0)
    val board  = Board.fromValues(values).toOption.get
    assertEquals(Solver.solve(board).left.toOption, Some(SolveError.NoSolution))

  test("branches when logic alone is not enough"):
    val result = Solver.solve(load("empty.txt")).toOption.get
    assert(result.events.exists { case Event.Forked(_, digits) => digits.size >= 2; case _ => false })

  test("every Placed event lists only newly decided cells with a consistent board"):
    val result = Solver.solve(load("medium-1.txt")).toOption.get
    result.events.foreach:
      case Event.Placed(cells, board) =>
        assert(cells.nonEmpty)
        cells.foreach((c, d) => assertEquals(board(c).single, Some(d)))
      case _ => ()

  test("an advisor is consulted when stuck, and can avoid guessing"):
    val puzzle  = load("hard.txt")
    val known   = Solver.solve(puzzle).toOption.get.board
    var asked   = 0
    val advisor: Advisor = board =>
      asked += 1
      val cell = board.unsolved.head
      Some(board.assign(cell, known(cell).single.get))
    val result = Solver.solve(puzzle, Some(advisor)).toOption.get
    assertSolutionFor(puzzle, result.board)
    assertEquals(result.board, known)
    assert(asked > 0 || puzzle.unsolved.isEmpty)
    assert(!result.events.exists(_.isInstanceOf[Event.Forked]))

  test("an advisor returning None falls back to guessing"):
    val puzzle = load("empty.txt")
    val result = Solver.solve(puzzle, Some(_ => None)).toOption.get
    assert(result.board.isSolved)

  test("render describes events"):
    val result = Solver.solve(load("empty.txt")).toOption.get
    val texts  = result.events.map(render)
    assert(texts.exists(_.startsWith("In this step")))
    assert(texts.exists(_.startsWith("Forked")))
