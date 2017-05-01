package name.lorenzani.andrea.sudoku

import name.lorenzani.andrea.sudoku.solver.SolverImpl


object Solver extends App {
  require(args.length > 0, "No filename passed")
  val solver = SolverImpl(args)
  solver.compute.foreach(println)
}
