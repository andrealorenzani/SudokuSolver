// SPDX-License-Identifier: Apache-2.0
// Copyright 2017-2026 Andrea Lorenzani

package name.lorenzani.andrea.sudoku

import scala.util.Try

/** A hint typed by a person to help the solver. Coordinates are 0-based, `x` is the column and `y` the row. */
enum Command:
  case Info(at: Coord)
  case Assign(at: Coord, digit: Int)
  case Remove(at: Coord, digit: Int)

object Command:

  val help: String =
    """Try to help the algorithm. Use the following commands:
      |	i x y      <-- display the possible values of the cell (x, y)
      |	+ x y val  <-- assign val (1 to 9) to (x, y)
      |	- x y val  <-- remove the possibility val (1 to 9) from (x, y)
      |	? or h     <-- show this help""".stripMargin

  def parse(line: String): Either[String, Command] =
    line.trim.split("\\s+").toList match
      case "i" :: x :: y :: Nil          => coord(x, y).map(Info(_))
      case "+" :: x :: y :: d :: Nil     => for c <- coord(x, y); v <- digit(d) yield Assign(c, v)
      case "-" :: x :: y :: d :: Nil     => for c <- coord(x, y); v <- digit(d) yield Remove(c, v)
      case ("i" | "+" | "-") :: _        => Left("Wrong number of arguments. Use ? for help")
      case other                         => Left(s"Command not found: ${other.mkString(" ")}")

  private def int(name: String, s: String): Either[String, Int] =
    s.toIntOption.toRight(s"$name is not a valid integer: $s")

  private def coord(x: String, y: String): Either[String, Coord] =
    for
      xi <- int("x", x)
      yi <- int("y", y)
      c  <- Try(Coord(xi, yi)).toEither.left.map(_ => s"x and y should be between 0 and 8 inclusive: ($xi, $yi)")
    yield c

  private def digit(s: String): Either[String, Int] =
    int("val", s).filterOrElse(d => d >= 1 && d <= 9, "val should be between 1 and 9")
