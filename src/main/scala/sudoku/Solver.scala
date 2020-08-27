package sudoku

object Solver {

  def solve(puzzle: Puzzle): Option[Puzzle] = {
    val updatedPuzzle = updatePuzzle(puzzle)
    if (updatedPuzzle.isImpossible) { None }
    else if (updatedPuzzle.isSolved) { Some(updatedPuzzle) }
    else {
      val backtrackIndex = updatedPuzzle.getIndexWithLeastOptions
      val fieldOptions = updatedPuzzle getForIndex backtrackIndex match { case Free(options) => options }
      val puzzleOptions = fieldOptions map (value => updatedPuzzle updateIndexWithValue(backtrackIndex, Filled(value)))
      (puzzleOptions flatMap solve).headOption
    }
  }

  def updatePuzzle(puzzle: Puzzle): Puzzle = {
    val rowIndexLists = (0 until 9) map Puzzle.rowIndices
    val rowsUpdated = updateIndexLists(rowIndexLists, puzzle)
    val colIndexLists = (0 until 9) map Puzzle.colIndices
    val colsUpdated = updateIndexLists(colIndexLists, rowsUpdated)
    val squareIndexLists = (0 until 9) map Puzzle.squareIndices
    updateIndexLists(squareIndexLists, colsUpdated)
  }

  def updateIndexLists(indexLists: Seq[Seq[Int]], puzzle: Puzzle): Puzzle = {
    val fieldLists = indexLists map puzzle.getForIndices
    val updatedFieldLists = fieldLists map updateFields
    (indexLists zip updatedFieldLists).foldRight(puzzle)(
      { case ((indices, fields), currentPuzzle) => currentPuzzle.updateIndicesWithValues(indices, fields) }
    )
  }

  def updateFields(fields: Seq[Field]): Seq[Field] = {
    val filledNumbers = fields filter (_.isFilled) map { case Filled(value) => value}
    fields map {
      case Free(options) =>
        val updatedOptions = options -- filledNumbers
        Free(updatedOptions)
      case Filled(value) => Filled(value)
    }
  }
}
