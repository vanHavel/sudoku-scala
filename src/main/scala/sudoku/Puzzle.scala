package sudoku

import sudoku.Puzzle.{SUDOKU_CELL_COUNT, colIndices, rowIndices, squareIndices}

case class Puzzle(state: Vector[Field]) {

  require(state.length == SUDOKU_CELL_COUNT)
  require(this.isValid)

  def getForIndex(index: Int): Field = state(index)

  def getForIndices(indices: Seq[Int]): Seq[Field] = indices map getForIndex

  def updateIndexWithValue(index: Int, value: Field): Puzzle = new Puzzle(state.updated(index, value))

  def updateIndicesWithValues(indices: Seq[Int], values: Seq[Field]): Puzzle =
    (indices zip values).foldRight(this)(
      { case ((index, value), puzzle) => puzzle.updateIndexWithValue(index, value) }
    )

  def isSolved: Boolean = state forall (_.isFilled)

  def isImpossible: Boolean = state exists (_.isImpossible)

  private def isValid: Boolean = {
    def hasUniqueFilledFields(fields: Seq[Field]): Boolean = {
      val filled = fields filter (_.isFilled)
      filled.toSet.size == filled.length
    }

    (0 until 9).flatMap(i => Seq(rowIndices(i), colIndices(i), squareIndices(i)))
      .map(this.getForIndices)
      .forall(hasUniqueFilledFields)
  }

  def getIndexWithLeastOptions: Int = {
    val freeFields = state.zipWithIndex
      .filter { case (field, _) => !field.isFilled }
      .map { case (Free(options), index) => (options, index) }
    val (_, index) = freeFields.minBy({case (options, _) => options.size})
    index
  }

  override def toString: String = ((0 until 9) flatMap (
      row => getForIndices(Puzzle.rowIndices(row)) flatMap (_.toString)
    )).toString

  def prettyPrint: String = {
    def intersperse[A](each: Int, value: A, sequence: Seq[A]): Seq[A] = {
      def intersperseAt[B](each: Int, at: Int, value: B, sequence: Seq[B]): Seq[B] =
        if (sequence.isEmpty) sequence else at match {
          case 0 => value +: intersperseAt(each, each, value, sequence)
          case i => sequence.head +: intersperseAt(each, i - 1, value, sequence.tail)
        }
      intersperseAt(each, each, value, sequence)
    }

    val rows = (0 to 8) map (row => getForIndices(Puzzle.rowIndices(row)).flatMap(_.prettyPrint))
    val prettyRows = rows map (row => intersperse(3, '|', row)) map (_.mkString)
    val prettyGrid = intersperse(3, "-----------", prettyRows)
    prettyGrid.reduce(_ ++ "\n" ++ _)
  }
}

object Puzzle {

  final val SUDOKU_CELL_COUNT = 81
  final val SUDOKU_CELLS_PER_SECTION = 9

  def rowIndices(row: Int): Seq[Int] = (SUDOKU_CELLS_PER_SECTION * row) until (SUDOKU_CELLS_PER_SECTION * (row + 1))

  def colIndices(col: Int): Seq[Int] = col until SUDOKU_CELL_COUNT by SUDOKU_CELLS_PER_SECTION

  def squareIndices(square: Int): Seq[Int] = {
    val squareStart = 3 * SUDOKU_CELLS_PER_SECTION * (square / 3) + 3 * (square % 3)
    val rowStarts = squareStart to (squareStart + 2 * SUDOKU_CELLS_PER_SECTION) by SUDOKU_CELLS_PER_SECTION
    rowStarts flatMap (index => index to (index + 2))
  }

  def fromString(string: String): Puzzle = Puzzle(string.toVector.map(Field.fromChar))

}
