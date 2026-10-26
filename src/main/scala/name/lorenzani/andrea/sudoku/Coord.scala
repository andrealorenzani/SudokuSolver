// SPDX-License-Identifier: Apache-2.0
// Copyright 2017-2026 Andrea Lorenzani

package name.lorenzani.andrea.sudoku

/** A cell position: `x` is the column and `y` the row, both from 0 to 8. */
final case class Coord(x: Int, y: Int):
  require(x >= 0 && x < 9 && y >= 0 && y < 9, s"Coordinates must be between 0 and 8 inclusive: ($x, $y)")

  def index: Int = y * 9 + x
  def box: Int   = (y / 3) * 3 + x / 3

  /** All the cells sharing a row, a column or a box with this one (excluding itself). */
  def peers: List[Coord] = Coord.peerTable(index)

  override def toString: String = s"($x, $y)"

object Coord:
  val all: Vector[Coord] = Vector.tabulate(81)(i => Coord(i % 9, i / 9))

  def fromIndex(i: Int): Coord = all(i)

  /** Rows, columns and boxes: the 27 groups that must each contain the digits 1-9 exactly once. */
  val units: Vector[Vector[Coord]] =
    val rows  = Vector.tabulate(9)(y => Vector.tabulate(9)(x => Coord(x, y)))
    val cols  = Vector.tabulate(9)(x => Vector.tabulate(9)(y => Coord(x, y)))
    val boxes = Vector.tabulate(9)(b => all.filter(_.box == b))
    rows ++ cols ++ boxes

  private val peerTable: Vector[List[Coord]] =
    all.map(c => all.filter(o => o != c && (o.x == c.x || o.y == c.y || o.box == c.box)).toList)
