package sudoku

abstract class Field {
  def isFilled: Boolean
  def isImpossible: Boolean
  def prettyPrint: String
}

object Field {
  final val VALID_CHARS = ('0' to '9').toSet
  def fromChar(c: Char): Field = {
    require(VALID_CHARS contains c)
    c match {
      case '0' => Free((1 to 9).toSet)
      case digit => Filled(digit - '0')
    }
  }
}

case class Filled(value: Int) extends Field {
  override def isFilled: Boolean = true
  override def isImpossible: Boolean = false
  override def toString: String = value.toString
  override def prettyPrint: String = value.toString
}
case class Free(options: Set[Int]) extends Field {
  override def isFilled: Boolean = false
  override def isImpossible: Boolean = options.isEmpty
  override def toString: String = options.toString()
  override def prettyPrint: String = " "
}
