// SPDX-License-Identifier: Apache-2.0
// Copyright 2017-2026 Andrea Lorenzani

package name.lorenzani.andrea.sudoku

import java.nio.file.Path

import scala.io.StdIn

private val Separator = "\n" + "_" * 64

def render(event: Event): String = event match
  case Event.Placed(cells, board) =>
    val lines = cells.map((c, d) => s"\t\t$c -> $d").mkString("\n")
    s"In this step ${cells.size} element reached the final state\n$lines\n\n$board$Separator"
  case Event.Forked(cell, digits) =>
    s"Forked ${digits.size} branches on $cell for coping with a non deterministic solution"

/** Asks for commands on stdin until one of them changes the board. `None` when the input ends. */
def keyboardAdvisor(board: Board): Option[Board] =
  println(Command.help)
  Iterator
    .continually(Option(StdIn.readLine()))
    .takeWhile(_.isDefined)
    .flatten
    .flatMap: line =>
      Command.parse(line) match
        case Left(_) if Set("?", "h")(line.trim) => println(Command.help); None
        case Left(error)                         => println(error); None
        case Right(Command.Info(at))             => println(s"$at: ${board(at).digits.mkString(", ")}"); None
        case Right(Command.Assign(at, digit))    => Some(board.assign(at, digit))
        case Right(Command.Remove(at, digit))    => Some(board.eliminate(at, digit))
    .nextOption()

/** Usage: `solve [-i] <file>`. */
@main def solve(args: String*): Unit =
  val interactive = args.contains("-i")
  args.filterNot(_ == "-i").lastOption match
    case None =>
      Console.err.println("No filename passed. Usage: solve [-i] <file>")
      sys.exit(2)
    case Some(file) =>
      val outcome = SudokuReader.read(Path.of(file)).flatMap: board =>
        Solver
          .solve(board, Option.when(interactive)(keyboardAdvisor))
          .left.map:
            case SolveError.Invalid    => "The Sudoku contains clashing digits"
            case SolveError.NoSolution => "The Sudoku has no solution"
      outcome match
        case Right(solution) => solution.events.map(render).foreach(println)
        case Left(error) =>
          Console.err.println(error)
          sys.exit(1)
