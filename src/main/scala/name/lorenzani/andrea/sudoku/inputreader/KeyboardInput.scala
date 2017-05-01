package name.lorenzani.andrea.sudoku.inputreader

import java.io.InputStream
import java.util.Scanner

import name.lorenzani.andrea.sudoku.inputreader.KeyboardInput.{Assign, Info, Remove}

import scala.annotation.tailrec
import scala.util.{Success, Try}

class KeyboardInput(is: InputStream) {
  private val keyBoard: Scanner = new Scanner(is)

  def showHelp: Unit = {
    println("Try to help the algorithm. Use the following commands:")
    println("\ti x y      \t<-- This will display the possible values of the cell (x,y)")
    println("\t+ x y val  \t<-- This will assign val (a value from 1 to 9) to (x,y)")
    println("\t- x y val  \t<-- This will remove a possibility to (x, y)")
    println("\t? or h     \t<-- This will show this help")
  }

  @tailrec
  final def getInput: Any = {

    def validateInput(in: List[String]) = {
      require("i+-".contains(in(0)), s"Command not found: ${in(0)}")
      require(Try { in(1).toInt }.isSuccess, s"x is not a valid integer: ${in(1)}")
      require(Try { in(2).toInt }.isSuccess, s"y is not a valid integer: ${in(2)}")
      val (x, y) = (in(1).toInt, in(2).toInt)
      require(x < 9 && x >= 0 && y >= 0 && y < 9, s"Value x and y should be between 0 and 8 inclusive: ($x, $y)")
      (x, y)
    }

    var input: List[String] = keyBoard.nextLine.split(" ").toList
    val res = Try {
      input.head match {
        case "i" =>
          val (x, y) = validateInput(input)
          require(input.size == 3, "Usage: i x y")
          Info(x, y)
        case "+" =>
          val (x, y) = validateInput(input)
          require(input.size == 4, "Usage: + x y val")
          require(input(3).toInt < 9 && input(3).toInt > 0, "val should be between 1 and 9")
          Assign(x, y, input(3).toInt)
        case "-" =>
          require(input.size == 4, "Usage: - x y val")
          val (x, y) = validateInput(input)
          require(input(3).toInt < 9 && input(3).toInt > 0, "val should be between 1 and 9")
          Remove(x, y, input(3).toInt)
        case _ => showHelp
      }
    }
    res match {
      case Success(x) => x
      case _ => getInput
    }
  }
}

object KeyboardInput {
  case class Info(x: Int, y: Int)
  case class Assign(x: Int, y: Int, value: Int)
  case class Remove(x: Int, y: Int, value: Int)
}
