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

class Sudoku private () {
  def position(n: Int): (Int, Int) = (n%9, n/9)
  def position(x: Int, y: Int): Int = y*9+x

  private var values: Array[SValue] = new Array[SValue](81)

  def apply(x: Int, y: Int): SValue = {
    require(x < 9 && x >= 0, s"Illegal apply on a Sudoku: x cannot be $x")
    require(y < 9 && y >= 0, s"Illegal apply on a Sudoku: y cannot be $y")
    values(x + (y * 9))
  }

  def update(x: Int, y: Int, value: Int): Unit = this(x, y).remove(value)
  def update(x: Int, y: Int, value: SValue): Unit = values(position(x, y)) = value
  def getRow(x: Int, y: Int): List[(Int, Int, SValue)] = (for(i <- 0 to 8) yield (i, y, this(i, y))).toList
  def getColumn(x: Int, y: Int): List[(Int, Int, SValue)] = (for(i <- 0 to 8) yield (x, i, this(x, i))).toList
  def getSector(x: Int, y: Int): List[(Int, Int, SValue)] = (for(i <- 0 to 2; j <- 0 to 2) yield ((x/3)*3+i, (y/3)*3+j, this((x/3)*3+i, (y/3)*3+j))).toList
  def updateRow(x: Int, y: Int, value: Int): Unit = getRow(x, y).foreach { case (_, _, svalue) => svalue.remove(value) }
  def updateColumn(x: Int, y: Int, value: Int): Unit = getColumn(x,y).foreach { case (_, _, svalue) =>  svalue.remove(value) }
  def updateSector(x: Int, y: Int, value: Int): Unit = getSector(x, y).foreach { case (_, _, svalue) => svalue.remove(value) }

  def solve: List[(Int, Int)] = {
    // This method is only confirming all the OptionalValues that are FinalValues (= they have only one option)
    // and updating the related (by column, row or sector) OptionalValues by removing
    // the new FinalValue values
    values.zipWithIndex.foldLeft(List[(Int, Int)]()) { (mutated, value) =>
      value match {
        case (v: OptionalValue, rawpos: Int) =>
          val newVal = v.mutate
          if(newVal != v) {
            val (x, y) = position(rawpos)
            val nv = newVal.getVal
            updateRow(x, y, nv)
            updateColumn(x, y, nv)
            updateSector(x, y, nv)
            values(rawpos) = newVal
            mutated :+ (x, y)
          }
          else mutated
        case _ => mutated
      }
    }
  }

  def search(these: List[(Int, Int, SValue)]): Boolean = {
    // This method applies the rule that 9 related values should contain values from 1 to 9
    // We can use this method by row, by column or by sector, depending on the input
      val found = these.map{
        case (_, _, n: FinalValue) => n.getVal
        case _ => 0
      }.toSet
      (1 to 9).filterNot(found.contains).map { hasSingle =>
        val contained = these.collect{ case elem@(x, y, value: OptionalValue) if value.remainingValues.contains(hasSingle) => elem }
        if(contained.size != 1) false else {
          val (x, y, newval) = contained.head
          this(x, y) = OptionalValue(hasSingle)
          true
        }
      }.exists(b => b)
  }

  def applyLogic[T, R](transform: List[(Int, Int, SValue)] => T, reduce: List[T] => R): R = {
    val byColumn = for(x <- 0 to 8) yield transform(getColumn(x, 0))
    val byRow = for(y <- 0 to 8) yield transform(getRow(0, y))
    val bySector = for(x <- 0 to 2; y <- 0 to 2) yield transform(getSector(x*3, y*3))
    reduce(byColumn.toList ++ byRow.toList ++ bySector.toList)
  }

  def search: Boolean = applyLogic[Boolean, Boolean](search, x => x.exists(x => x))
  def isInvalid: Boolean = applyLogic[List[Int], Boolean](list => (1 to 9).map(n=> list.count{ case (_, _, svalue) => svalue.getVal == n }).toList,
                                                          list => list.flatten.exists(n => n>1))

  def getNumOptions(list: List[(Int, Int, SValue)]): List[(Int, Int, Int)] = list.collect{ case (x, y, sv: OptionalValue) => (x, y, sv.remainingValues.size)}
  def getMinimal: (Int, Int, OptionalValue) = applyLogic[List[(Int, Int, Int)], (Int, Int, OptionalValue)](getNumOptions, value => {
    val min = value.flatten.minBy(x => x._3)
    (min._1, min._2, this(min._1, min._2).asInstanceOf[OptionalValue])
  })

  def spawn: Sudoku = {
    val res = new Sudoku()
    res.values = values.map(_.copy)
    res
  }

  def isEnded: Boolean = !values.exists(_.isInstanceOf[OptionalValue])


  override def toString: String = (for(y <- 0 to 8) yield {
    val res = (for(x <- 0 to 8) yield {
        val res = values(x+(y*9)).toString
        if(x==2 || x==5) s"$res " else res
      }).mkString("")
    if(y==2 || y==5) s"$res\n" else res
  }).mkString("\n")
}

object Sudoku {
  def apply(values: List[Int]): Sudoku = {
    require(values.size == 81, s"Wrong number of elements (a Sudoku has 81 values): ${values.size}")
    var sud = new Sudoku()
    sud.values = values.map(x => OptionalValue(x)).toArray
    sud
  }
}
