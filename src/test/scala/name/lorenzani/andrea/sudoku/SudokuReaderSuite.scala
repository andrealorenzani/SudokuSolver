// SPDX-License-Identifier: Apache-2.0
// Copyright 2017-2026 Andrea Lorenzani

package name.lorenzani.andrea.sudoku

class SudokuReaderSuite extends munit.FunSuite:

  private val row = "123456789"

  test("parses a grid, ignoring blank lines"):
    val lines = List.fill(9)(row).flatMap(l => List(l, ""))
    assertEquals(SudokuReader.parse(lines).map(_.values), Right(List.fill(9)((1 to 9).toList).flatten))

  test("accepts dots, stars and underscores as empty cells"):
    val lines = List("1.*_00000") ++ List.fill(8)("000000000")
    assertEquals(SudokuReader.parse(lines).map(_.values.take(9)), Right(List(1, 0, 0, 0, 0, 0, 0, 0, 0)))

  test("rejects the wrong number of lines"):
    assert(SudokuReader.parse(List.fill(8)(row)).isLeft)
    assert(SudokuReader.parse(List.fill(10)(row)).isLeft)

  test("rejects lines of the wrong length"):
    assert(SudokuReader.parse(List.fill(8)(row) :+ "12345678").isLeft)
    assert(SudokuReader.parse(List.fill(8)(row) :+ "1234567890").isLeft)

  test("rejects unexpected characters"):
    assert(SudokuReader.parse(List.fill(8)(row) :+ "12345678x").isLeft)

  test("reads a puzzle from disk"):
    val board = SudokuReader.read(java.nio.file.Path.of("puzzles/easy-1.txt"))
    assertEquals(board.map(_.unsolved.size), Right(57))

  test("reports a missing file"):
    assert(SudokuReader.read(java.nio.file.Path.of("puzzles/nope.txt")).isLeft)
