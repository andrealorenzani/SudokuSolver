// SPDX-License-Identifier: Apache-2.0
// Copyright 2017-2026 Andrea Lorenzani

package name.lorenzani.andrea.sudoku

class BoardSuite extends munit.FunSuite:

  test("coordinates map to indexes, boxes and back"):
    assertEquals(Coord(5, 1).index, 14)
    assertEquals(Coord.fromIndex(14), Coord(5, 1))
    assertEquals(Coord(4, 4).box, 4)
    assertEquals(Coord(8, 0).box, 2)
    assertEquals(Coord(0, 8).box, 6)

  test("coordinates are validated"):
    intercept[IllegalArgumentException](Coord(9, 0))
    intercept[IllegalArgumentException](Coord(0, -1))

  test("every cell has 20 distinct peers"):
    Coord.all.foreach: c =>
      assertEquals(c.peers.size, 20)
      assertEquals(c.peers.distinct.size, 20)
      assert(!c.peers.contains(c))

  test("there are 27 units of 9 cells"):
    assertEquals(Coord.units.size, 27)
    assert(Coord.units.forall(_.distinct.size == 9))
    assertEquals(Coord.units(18).map(c => (c.x, c.y)).toList.take(3), List((0, 0), (1, 0), (2, 0)))

  test("fromValues validates its input"):
    assert(Board.fromValues(List.fill(80)(0)).isLeft)
    assert(Board.fromValues(List.fill(82)(0)).isLeft)
    assert(Board.fromValues(List.fill(81)(10)).isLeft)
    assert(Board.fromValues(List.fill(81)(-1)).isLeft)
    assert(Board.fromValues(List.fill(81)(0)).isRight)

  test("values round-trips"):
    val values = (0 until 81).map(i => i % 10).toList
    assertEquals(Board.fromValues(values).map(_.values), Right(values))

  test("assign and eliminate return updated copies"):
    val b = Board.empty.assign(Coord(2, 3), 5)
    assertEquals(b(Coord(2, 3)).single, Some(5))
    assertEquals(Board.empty(Coord(2, 3)), Candidates.all)
    val e = Board.empty.eliminate(Coord(0, 0), 9)
    assert(!e(Coord(0, 0)).contains(9))

  test("empty board is neither solved nor contradictory"):
    assert(!Board.empty.isSolved)
    assert(!Board.empty.isContradiction)
    assertEquals(Board.empty.unsolved.size, 81)

  test("a digit repeated in a unit is a conflict"):
    val b = Board.empty.assign(Coord(0, 0), 1).assign(Coord(8, 0), 1)
    assert(b.hasConflict)
    assert(b.isContradiction)
    val diagonal = Board.empty.assign(Coord(0, 0), 1).assign(Coord(8, 8), 1)
    assert(!diagonal.isContradiction)

  test("a cell without candidates is a contradiction"):
    val b = (1 to 9).foldLeft(Board.empty)((acc, d) => acc.eliminate(Coord(4, 4), d))
    assert(b.isContradiction)

  test("a board with the same digit everywhere is conflicting but fully decided"):
    val b = Board.fromValues(List.fill(81)(1)).toOption.get
    assert(b.unsolved.isEmpty)
    assert(b.hasConflict)
    assert(!b.isSolved)

  test("show draws the grid in boxes"):
    val b = Board.fromValues(List.tabulate(81)(i => if i == 0 then 7 else 0)).toOption.get
    val lines = b.show.split("\n")
    assertEquals(lines.length, 11)
    assertEquals(lines.head, "7** *** ***")
    assertEquals(lines(3), "")
