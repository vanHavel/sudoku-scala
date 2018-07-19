object Sudoku {

  def main(args: Array[String] ) {
    val puzzle = parsePuzzle
    Solver.solve(puzzle) match {
      case None => println("Puzzle has no solution.")
      case Some(solution) => printPuzzle(solution)
    }
  }

  def parsePuzzle: Puzzle = {
    val input = scala.io.StdIn.readLine()
    val fieldVector = (input map Field.fromChar).toVector
    Puzzle(fieldVector)
  }

  def printPuzzle(puzzle: Puzzle): Unit = print(puzzle.toString)
}
