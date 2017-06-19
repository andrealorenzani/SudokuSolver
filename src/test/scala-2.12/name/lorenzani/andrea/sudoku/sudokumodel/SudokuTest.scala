/***
*   Copyright 2017 Andrea Lorenzani
*
*   Licensed under the Apache License, Version 2.0 (the "License");
*   you may not use this file except in compliance with the License.
*   You may obtain a copy of the License at
*
*       http://www.apache.org/licenses/LICENSE-2.0
*
*   Unless required by applicable law or agreed to in writing, software
*   distributed under the License is distributed on an "AS IS" BASIS,
*   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
*   See the License for the specific language governing permissions and
*   limitations under the License.
*
***/

package name.lorenzani.andrea.sudoku.sudokumodel

import org.scalatest.FlatSpec

import scala.util.Random

class SudokuTest extends FlatSpec {

  "Sudoku" should "be created properly" in {
    assertThrows[IllegalArgumentException](Sudoku((1 to 82).map(x => 1).toList))
    assertThrows[IllegalArgumentException](Sudoku((1 to 80).map(x => 1).toList))
    assertThrows[IllegalArgumentException](Sudoku((1 to 81).toList))
    val sudoku = Sudoku((1 to 81).map(x => 1).toList)
  }

  it should "give internal status info properly" in {
    val sudoku = Sudoku((1 to 81).map(_ => 0).toList)
    for(n <- 0 until 81) {
      val (x, y) = sudoku.position(n)
      sudoku(x, y) = FinalValue(1).asInstanceOf[SValue]
    }
    assert(sudoku.isInvalid)
    assert(sudoku.isEnded)
    val sudoku2 = Sudoku((1 to 81).map(x => 0).toList)
    assert(!sudoku2.isInvalid)
    assert(!sudoku2.isEnded)
  }

  it should "update optional values accordingly" in {
    val sudoku = Sudoku((1 to 81).map(x => 0).toList)
    val (x, y) = (Random.nextInt(9), Random.nextInt(9))
    assert(sudoku.getColumn(x, y).forall(sv => sv._3.asInstanceOf[OptionalValue].remainingValues.size == 9))
    assert(sudoku.getRow(x, y).forall(sv => sv._3.asInstanceOf[OptionalValue].remainingValues.size == 9))
    assert(sudoku.getSector(x, y).forall(sv => sv._3.asInstanceOf[OptionalValue].remainingValues.size == 9))
    sudoku(x, y) = OptionalValue(7)
    val solved = sudoku.solve
    assert(solved.size == 1)
    assert(sudoku.getColumn(x, y).filterNot(sv => (sv._1, sv._2) == (x,y)).forall(sv => sv._3.asInstanceOf[OptionalValue].remainingValues.size == 8))
    assert(sudoku.getRow(x, y).filterNot(sv => (sv._1, sv._2) == (x,y)).forall(sv => sv._3.asInstanceOf[OptionalValue].remainingValues.size == 8))
    assert(sudoku.getSector(x, y).filterNot(sv => (sv._1, sv._2) == (x,y)).forall(sv => sv._3.asInstanceOf[OptionalValue].remainingValues.size == 8))
    assert(sudoku.getColumn(x, y).filterNot(sv => (sv._1, sv._2) == (x,y)).forall(sv => !sv._3.asInstanceOf[OptionalValue].remainingValues.contains(7)))
    assert(sudoku.getRow(x, y).filterNot(sv => (sv._1, sv._2) == (x,y)).forall(sv => !sv._3.asInstanceOf[OptionalValue].remainingValues.contains(7)))
    assert(sudoku.getSector(x, y).filterNot(sv => (sv._1, sv._2) == (x,y)).forall(sv => !sv._3.asInstanceOf[OptionalValue].remainingValues.contains(7)))
  }

  it should "handle as expected coordinates" in {
    val sudoku = Sudoku((1 to 81).map(x => 0).toList)
    assert(sudoku.position(14) == (5, 1))
    assert(sudoku.position(5, 1) == 14)
    val listValuesSector1 = List((0,0), (0,1), (0,2), (1,0), (1,1), (1,2), (2,0), (2,1), (2,2))
    assert(listValuesSector1 == sudoku.getSector(1, 1).map(x => (x._1, x._2)))
    assert((0 to 8).map(x => (x, 1)).toList == sudoku.getRow(1, 1).map(x => (x._1, x._2)))
    assert((0 to 8).map(y => (1, y)).toList == sudoku.getColumn(1, 1).map(x => (x._1, x._2)))
  }

}
