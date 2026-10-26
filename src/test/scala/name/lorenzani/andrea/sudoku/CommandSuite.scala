// SPDX-License-Identifier: Apache-2.0
// Copyright 2017-2026 Andrea Lorenzani

package name.lorenzani.andrea.sudoku

import Command.*

class CommandSuite extends munit.FunSuite:

  test("parses the three commands"):
    assertEquals(Command.parse("i 1 2"), Right(Info(Coord(1, 2))))
    assertEquals(Command.parse("+ 0 8 9"), Right(Assign(Coord(0, 8), 9)))
    assertEquals(Command.parse("- 4 4 1"), Right(Remove(Coord(4, 4), 1)))

  test("tolerates extra whitespace"):
    assertEquals(Command.parse("  +   3  3   3 "), Right(Assign(Coord(3, 3), 3)))

  test("the digit 9 is accepted (the 2017 version rejected it)"):
    assert(Command.parse("+ 1 1 9").isRight)
    assert(Command.parse("- 1 1 9").isRight)

  test("rejects invalid input"):
    List("", "x", "? ", "i 1", "i 1 2 3", "+ 1 2", "+ 1 2 3 4", "i a 2", "i 1 b", "i 9 0", "i 0 -1", "+ 1 1 0", "- 1 1 10", "+ 1 1 x")
      .foreach(line => assert(Command.parse(line).isLeft, s"should reject '$line'"))
