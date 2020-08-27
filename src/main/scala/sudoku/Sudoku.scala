package sudoku

object Sudoku {

  def main(args: Array[String]) {
    val input = readAllInput
    parsePuzzle(input) match {
      case None => println("Invalid puzzle input.")
      case Some(puzzle) => Solver.solve(puzzle) match {
        case None => println("Puzzle has no solution.")
        case Some(solution) => printPuzzle(solution)
      }
    }
  }

  def parsePuzzle(input: String): Option[Puzzle] = {
    val fieldVector = (input filter (Field.VALID_CHARS.contains) map Field.fromChar).toVector
    if (fieldVector.length != Puzzle.SUDOKU_CELL_COUNT) None else Some(Puzzle(fieldVector))
  }

  def readAllInput: String = {
    val line = scala.io.StdIn.readLine
    if (line != null && line.nonEmpty) (line ++ readAllInput) else ""
  }

  def printPuzzle(puzzle: Puzzle): Unit = print(puzzle.prettyPrint)
}
