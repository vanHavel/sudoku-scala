object Solver {

  def solve(puzzle: Puzzle): Option[Puzzle] = {
    val updatedPuzzle = updatePuzzle(puzzle)

    if (updatedPuzzle isImpossible) { None }
    else if (updatedPuzzle != puzzle) { solve(updatedPuzzle) }
    else if (updatedPuzzle isSolved) { Some(updatedPuzzle) }
    else {
      val backtrackIndex = updatedPuzzle getIndexWithLeastOptions
      val fieldOptions = updatedPuzzle getForIndex backtrackIndex match { case Free(options) => options }
      val puzzleOptions = fieldOptions map (value => updatedPuzzle updateIndexWithValue(backtrackIndex, Filled(value)))
      (puzzleOptions flatMap solve) headOption
    }
  }

  def updatePuzzle(puzzle: Puzzle): Puzzle = {
    val indexLists =
      (0 to 9) flatMap (index => Seq(Puzzle.rowIndices(index), Puzzle.colIndices(index), Puzzle.squareIndices(index)))
    val fieldLists = indexLists map puzzle.getForIndices
    val updatedFieldLists = fieldLists map updateFields
    (indexLists zip updatedFieldLists).foldRight(puzzle)(
      { case ((indices, fields), currentPuzzle) => currentPuzzle.updateIndicesWithValues(indices, fields) }
    )
  }

  def updateFields(fields: Seq[Field]): Seq[Field] = {
    val filledNumbers = fields filter (_.isFilled) map { case Filled(value) => value}
    fields map {
      case Free(options) => {
        val updatedOptions = options -- filledNumbers
        if (updatedOptions.size == 1) Filled(updatedOptions.head) else Free(updatedOptions)
      }
      case Filled(value) => Filled(value)
    }
  }
}
