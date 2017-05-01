package name.lorenzani.andrea.sudoku.solver

import name.lorenzani.andrea.sudoku.inputreader.KeyboardInput.{Assign, Info, Remove}
import name.lorenzani.andrea.sudoku.inputreader.{KeyboardInput, SudokuReader}
import name.lorenzani.andrea.sudoku.sudokumodel.{FinalValue, OptionalValue, Sudoku}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}
import scala.util.Success

class SolverImpl(val sudoku: Sudoku, val keyboard: Option[KeyboardInput]) {

  def compute: List[String] = {
    val nextStep = sudoku.solve
    if(sudoku.isInvalid) throw new IllegalStateException("Sudoku ended in an invalid state")
    val status = saveStatus(sudoku, nextStep)
    if(sudoku.isEnded) List(status)
    else {
      if(nextStep.nonEmpty || sudoku.search) List(status) ++ compute
      else keyboard match {
        case Some(kb) =>
          kb.getInput match {
            case Info(x, y) => sudoku(x, y) match {
              case ov: OptionalValue => println(s"($x, $y): ${ov.remainingValues.mkString(", ")}")
              case fv: FinalValue => println(s"($x, $y): $fv")
            }
            case Assign(x, y, value) => sudoku(x, y) = OptionalValue(value)
            case Remove(x, y, value) => sudoku(x, y) = value
          }
          List(status) ++ compute
        case None =>
          import scala.concurrent.ExecutionContext.Implicits.global
          val min = sudoku.getMinimal
          val parallel = min._3.remainingValues.map ( newval => Future {
            val newsudoku = sudoku.spawn
            newsudoku(min._1, min._2) = OptionalValue(newval)
            new SolverImpl(newsudoku, None).compute
          })
          val first = Promise[List[String]]()
          parallel.foreach(fut => fut.onComplete {
            case Success(x) => first.trySuccess(x)
            case _ =>
          } )
          Await.result(first.future, Duration.Inf)
          List(status, s"Forked into ${min._3.remainingValues.size} branches for coping with a non deterministic solution") ++ first.future.value.get.get
      }
    }
  }

  def saveStatus(sudoku: Sudoku, step: List[(Int, Int)]): String = {
    val sudokuStr = new StringBuffer()
    sudokuStr.append(s"In this step ${step.size} element reached the final state\n\r")
    step.map(vals => (vals._1, vals._2, (sudoku.apply _).tupled(vals).toString))
      .map { case (x, y, value) => s"($x, $y) -> $value" }
      .foreach(x => sudokuStr.append(s"\t\t$x\n\r"))
    sudokuStr.append("\n")
             .append(sudoku.toString)
             .append("\n________________________________________________________________\n")
    sudokuStr.toString
  }
}

object SolverImpl {
  def apply(args: Array[String]): SolverImpl = {
    val sudoku = SudokuReader(args)
    val keyboard = if(args.contains("-i")) Some(new KeyboardInput(System.in)) else None
    new SolverImpl(Sudoku(sudoku.getRawSudokuValues), keyboard)
  }
}
