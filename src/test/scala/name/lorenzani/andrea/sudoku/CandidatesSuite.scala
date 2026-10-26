// SPDX-License-Identifier: Apache-2.0
// Copyright 2017-2026 Andrea Lorenzani

package name.lorenzani.andrea.sudoku

class CandidatesSuite extends munit.FunSuite:

  test("all holds the digits 1 to 9"):
    assertEquals(Candidates.all.digits, (1 to 9).toList)
    assertEquals(Candidates.all.size, 9)

  test("of builds a single candidate and rejects other values"):
    assertEquals(Candidates.of(4).single, Some(4))
    intercept[IllegalArgumentException](Candidates.of(0))
    intercept[IllegalArgumentException](Candidates.of(10))

  test("removing a digit, present or not"):
    val c = Candidates.all - 7
    assert(!c.contains(7))
    assertEquals(c.size, 8)
    assertEquals(c - 7, c)
    assertEquals(c - 42, c)

  test("single, isSingle and isEmpty"):
    val almost = (2 to 9).foldLeft(Candidates.all)(_ - _)
    assertEquals(almost.single, Some(1))
    assert(almost.isSingle)
    assertEquals(Candidates.all.single, None)
    assert((almost - 1).isEmpty)
    assertEquals(Candidates.none.single, None)

  test("show prints the digit or a star"):
    assertEquals(Candidates.of(3).show, "3")
    assertEquals(Candidates.all.show, "*")
