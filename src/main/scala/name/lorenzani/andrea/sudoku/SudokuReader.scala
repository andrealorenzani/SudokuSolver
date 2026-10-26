// SPDX-License-Identifier: Apache-2.0
// Copyright 2017-2026 Andrea Lorenzani

package name.lorenzani.andrea.sudoku

import java.nio.file.{Files, Path}

import scala.util.Using

object SudokuReader:

  /** Parses 9 lines of 9 digits each, 0 (or any of `.`, `*`, `_`) standing for an empty cell. Blank lines are ignored. */
  def parse(lines: Iterable[String]): Either[String, Board] =
    val rows = lines.map(_.trim).filter(_.nonEmpty).toList
    if rows.size != 9 then Left(s"Wrong number of lines: ${rows.size}")
    else
      rows
        .map: row =>
          if row.length != 9 then Left(s"Line with wrong number of elements: $row")
          else row.toList.traverse(cell)
        .traverse(identity)
        .flatMap(grid => Board.fromValues(grid.flatten))

  def read(path: Path): Either[String, Board] =
    Using(Files.lines(path))(_.toArray.toList.map(_.toString))
      .toEither
      .left.map(e => s"Cannot read $path: ${e.getMessage}")
      .flatMap(parse)

  private def cell(ch: Char): Either[String, Int] =
    if ch.isDigit then Right(ch.asDigit)
    else if ".*_".contains(ch) then Right(0)
    else Left(s"Invalid character in the grid: '$ch'")

  extension [A](xs: List[A])
    private def traverse[B](f: A => Either[String, B]): Either[String, List[B]] =
      xs.foldRight[Either[String, List[B]]](Right(Nil))((a, acc) => for b <- f(a); bs <- acc yield b :: bs)
