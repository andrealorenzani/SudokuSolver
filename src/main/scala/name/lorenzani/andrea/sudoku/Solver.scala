// SPDX-License-Identifier: Apache-2.0
// Copyright 2017-2026 Andrea Lorenzani

package name.lorenzani.andrea.sudoku

import scala.annotation.tailrec

/** What happened while solving, in order. */
enum Event:
  /** Cells that reached their final value in one propagation round. */
  case Placed(cells: List[(Coord, Int)], board: Board)

  /** The solver was stuck and tried each digit of `cell` in turn. */
  case Forked(cell: Coord, digits: List[Int])

enum SolveError:
  /** The given digits already clash with each other. */
  case Invalid

  /** The givens are consistent but no complete grid satisfies them. */
  case NoSolution

final case class Solution(board: Board, events: List[Event])

/** Lets a human unblock the solver when plain logic gets stuck. Returning `None` makes the solver guess instead. */
type Advisor = Board => Option[Board]

object Solver:

  def solve(board: Board, advisor: Option[Advisor] = None): Either[SolveError, Solution] =
    if board.isContradiction then Left(SolveError.Invalid)
    else search(board, advisor).toRight(SolveError.NoSolution)

  /** Logical deduction first; when stuck, ask the advisor or branch on the most constrained cell. */
  private def search(board: Board, advisor: Option[Advisor]): Option[Solution] =
    propagate(board).flatMap { (reduced, events) =>
      if reduced.unsolved.isEmpty then Some(Solution(reduced, events))
      else
        advisor.flatMap(_(reduced)) match
          case Some(advised) => search(advised, advisor).map(s => s.copy(events = events ++ s.events))
          case None          => branch(reduced).map(s => s.copy(events = events ++ s.events))
    }

  private def branch(board: Board): Option[Solution] =
    val cell   = board.unsolved.minBy(board(_).size)
    val digits = board(cell).digits
    LazyList
      .from(digits)
      .flatMap(d => search(board.assign(cell, d), None))
      .headOption
      .map(s => s.copy(events = Event.Forked(cell, digits) :: s.events))

  /** Applies the two deduction rules until nothing changes. `None` means a contradiction was reached. */
  private def propagate(start: Board): Option[(Board, List[Event])] =
    @tailrec def loop(board: Board, events: List[Event]): Option[(Board, List[Event])] =
      val (eliminated, placed) = eliminateFromPeers(board)
      if eliminated.isContradiction then None
      else if placed.nonEmpty then loop(eliminated, Event.Placed(placed, eliminated) :: events)
      else
        hiddenSingles(eliminated) match
          case None => None
          case Some((next, hidden)) if hidden.nonEmpty =>
            if next.isContradiction then None else loop(next, Event.Placed(hidden, next) :: events)
          case Some(_) => Some((eliminated, events.reverse))
    loop(start, Nil)

  /** Naked singles: a solved cell rules its digit out of every peer. Returns the cells solved by doing so. */
  private def eliminateFromPeers(board: Board): (Board, List[(Coord, Int)]) =
    @tailrec def fixpoint(b: Board): Board =
      val next = Coord.all.foldLeft(b) { (acc, c) =>
        acc(c).single.fold(acc)(d => c.peers.foldLeft(acc)((a, p) => a.eliminate(p, d)))
      }
      if next == b then b else fixpoint(next)

    val after = fixpoint(board)
    val placed = Coord.all.collect:
      case c if !board(c).isSingle && after(c).isSingle => (c, after(c).single.get)
    (after, placed.toList)

  /** Hidden singles: a digit that fits only one cell of a row, column or box must go there.
    * `None` if some unit has no place left for a digit.
    */
  private def hiddenSingles(board: Board): Option[(Board, List[(Coord, Int)])] =
    val init: Option[(Board, List[(Coord, Int)])] = Some((board, Nil))
    (for unit <- Coord.units; digit <- 1 to 9 yield (unit, digit)).foldLeft(init) {
      case (None, _) => None
      case (Some((b, placed)), (unit, digit)) =>
        val spots = unit.filter(c => b(c).contains(digit))
        spots match
          case Vector()                                       => None
          case Vector(c) if !b(c).isSingle                    => Some((b.assign(c, digit), placed :+ (c, digit)))
          case _                                              => Some((b, placed))
    }
