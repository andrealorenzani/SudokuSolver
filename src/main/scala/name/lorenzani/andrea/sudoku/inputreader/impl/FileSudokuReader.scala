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

package name.lorenzani.andrea.sudoku.inputreader.impl

import name.lorenzani.andrea.sudoku.inputreader.SudokuReader

import scala.io.Source

class FileSudokuReader(file: String) extends SudokuReader {
  override def getRawSudokuValues: List[Int] = {
    val parsedLines = (for (line <- Source.fromFile(file).getLines()) yield {
      require(line.trim.length == 9, s"Line with wrong number of elements: $line")
      for (num <- line.trim) yield num.toString.toInt
    }).toList
    require(parsedLines.size == 9, s"Wrong number of lines: ${parsedLines.size}")
    parsedLines
  }.flatten
}
