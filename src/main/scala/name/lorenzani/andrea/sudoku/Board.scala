// SPDX-License-Identifier: Apache-2.0
// Copyright 2017-2026 Andrea Lorenzani

package name.lorenzani.andrea.sudoku

/** An immutable Sudoku grid: every cell holds the set of digits it can still take.
  * A cell with exactly one candidate is considered solved.
  */
final case class Board private (cells: Vector[Candidates]):

  def apply(c: Coord): Candidates = cells(c.index)

  /** Restricts a cell to a single digit. */
  def assign(c: Coord, digit: Int): Board = copy(cells = cells.updated(c.index, Candidates.of(digit)))

  /** Rules a digit out of a cell. */
  def eliminate(c: Coord, digit: Int): Board = copy(cells = cells.updated(c.index, this(c) - digit))

  def isSolved: Boolean = cells.forall(_.isSingle) && !hasConflict

  /** True when some cell has no candidate left or a digit is fixed twice in the same group. */
  def isContradiction: Boolean = cells.exists(_.isEmpty) || hasConflict

  def hasConflict: Boolean =
    Coord.units.exists { unit =>
      val fixed = unit.flatMap(c => this(c).single)
      fixed.distinct.size != fixed.size
    }

  /** Cells that are still undecided. */
  def unsolved: Vector[Coord] = Coord.all.filter(c => !this(c).isSingle)

  /** The 81 values in row-major order, 0 standing for an undecided cell. */
  def values: List[Int] = cells.map(_.single.getOrElse(0)).toList

  def show: String =
    (0 until 9).map { y =>
      val row = (0 until 9).map { x =>
        val cell = this(Coord(x, y)).show
        if x == 2 || x == 5 then s"$cell " else cell
      }.mkString
      if y == 2 || y == 5 then s"$row\n" else row
    }.mkString("\n")

  override def toString: String = show

object Board:
  val empty: Board = Board(Vector.fill(81)(Candidates.all))

  /** Builds a board from 81 row-major values, where 0 means "unknown". */
  def fromValues(values: Seq[Int]): Either[String, Board] =
    if values.size != 81 then Left(s"Wrong number of elements (a Sudoku has 81 values): ${values.size}")
    else
      values.find(v => v < 0 || v > 9) match
        case Some(bad) => Left(s"Values must be between 0 and 9: $bad")
        case None =>
          Right(Board(values.map(v => if v == 0 then Candidates.all else Candidates.of(v)).toVector))
