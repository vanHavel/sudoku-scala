package sudoku

object Sudoku {

  def main(args: Array[String]) {
    val input = readAllInput
      Solver.solve(Puzzle.fromString(input)) match {
        case None => println("Puzzle has no solution.")
        case Some(solution) => print(solution.prettyPrint)
      }
    }

  def readAllInput: String = {
    val line = scala.io.StdIn.readLine
    if (line != null && line.nonEmpty) (line ++ readAllInput) else ""
  }
}
