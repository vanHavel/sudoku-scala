object Sudoku {

  def main(args: Array[String] ) {
    val puzzle = parsePuzzle
    Solver.solve(puzzle) match {
      case None => println("Puzzle has no solution.")
      case Some(solution) => printPuzzle(solution)
    }
  }

  def parsePuzzle: Puzzle = {
    val fieldVector = (readAllInput map Field.fromChar).toVector
    Puzzle(fieldVector)
  }

  def readAllInput: String = {
    val line = scala.io.StdIn.readLine
    if (line.nonEmpty) {
      line ++ readAllInput
    }
    else {
      ""
    }
  }

  def printPuzzle(puzzle: Puzzle): Unit = print(puzzle.toString)
}
