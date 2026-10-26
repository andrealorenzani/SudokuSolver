// SPDX-License-Identifier: Apache-2.0
// Copyright 2017-2026 Andrea Lorenzani

package name.lorenzani.andrea.sudoku

/** The set of digits (1-9) still possible for a cell, packed in the bits 1..9 of an `Int`. */
opaque type Candidates = Int

object Candidates:
  val none: Candidates = 0
  val all: Candidates  = 0x3fe

  def of(digit: Int): Candidates =
    require(digit >= 1 && digit <= 9, s"A digit must be between 1 and 9: $digit")
    1 << digit

  extension (c: Candidates)
    def contains(digit: Int): Boolean = digit >= 1 && digit <= 9 && (c & (1 << digit)) != 0
    def -(digit: Int): Candidates     = if digit >= 1 && digit <= 9 then c & ~(1 << digit) else c
    def size: Int                     = Integer.bitCount(c)
    def isEmpty: Boolean              = c == 0
    def isSingle: Boolean             = size == 1
    def digits: List[Int]             = (1 to 9).filter(contains).toList
    def single: Option[Int]           = if isSingle then Some(Integer.numberOfTrailingZeros(c)) else None
    def show: String                  = single.fold("*")(_.toString)
