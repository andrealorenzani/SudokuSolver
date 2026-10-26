ThisBuild / scalaVersion := "3.10.0"
ThisBuild / version      := "2.0.0"
ThisBuild / organization := "name.lorenzani.andrea"
ThisBuild / licenses     := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0"))

lazy val root = (project in file("."))
  .settings(
    name := "sudoku-solver",
    scalacOptions ++= Seq("-deprecation", "-feature", "-Wunused:all", "-Wvalue-discard"),
    libraryDependencies += "org.scalameta" %% "munit" % "1.3.6" % Test,
    Compile / mainClass := Some("name.lorenzani.andrea.sudoku.solve"),
    Test / fork := true,
    // the solver tests read the puzzles in ./puzzles
    Test / baseDirectory := (ThisBuild / baseDirectory).value
  )
