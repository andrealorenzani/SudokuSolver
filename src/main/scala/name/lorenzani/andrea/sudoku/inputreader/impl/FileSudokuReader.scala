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
