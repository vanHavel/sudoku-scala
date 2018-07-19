case class Puzzle(state: Vector[Field]) {

  def getForIndex(index: Int): Field = state(index)

  def getForIndices(indices: Seq[Int]): Seq[Field] = indices map getForIndex

  def updateIndexWithValue(index: Int, value: Field): Puzzle = new Puzzle(state.updated(index, value))

  def updateIndicesWithValues(indices: Seq[Int], values: Seq[Field]): Puzzle =
    (indices zip values).foldRight(this)(
      { case ((index, value), puzzle) => puzzle.updateIndexWithValue(index, value) }
    )

  def isSolved: Boolean = state forall (_.isFilled)

  def isImpossible: Boolean = state exists (_.isImpossible)

  def getIndexWithLeastOptions: Int = {
    val freeFields = state.zipWithIndex filter (! _._1.isFilled) map { case (Free(options), index) => (options, index) }
    freeFields.minBy(_._1.size)._2
  }

  override def toString: String = ((1 to 9) flatMap (
      row => getForIndices(Puzzle.rowIndices(row)) flatMap (_.toString)
    )).toString

}

object Puzzle {

  def rowIndices(row: Int): Seq[Int] = (9 * row) until (10 * row)

  def colIndices(col: Int): Seq[Int] = col until 81 by 9

  def squareIndices(square: Int): Seq[Int] = {
    val squareStart = 27 * (square / 3) + 3 * (square % 3)
    val rowStarts = squareStart to (squareStart + 18) by 9
    rowStarts flatMap (index => index to (index + 2))
  }

}
