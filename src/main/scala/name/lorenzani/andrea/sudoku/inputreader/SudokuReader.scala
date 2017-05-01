package name.lorenzani.andrea.sudoku.inputreader

import name.lorenzani.andrea.sudoku.inputreader.impl.FileSudokuReader

trait SudokuReader {
  def getRawSudokuValues: List[Int]
}

object SudokuReader {
  def apply(args: Array[String]): SudokuReader = new FileSudokuReader(args.last)
}
