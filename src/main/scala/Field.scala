abstract class Field {
  def isFilled: Boolean
  def isImpossible: Boolean
}

object Field {
  def fromChar(c: Char): Field = c match {
    case '0' => Free((1 to 9).toSet)
    case digitCode => Filled(digitCode - '0')
  }
}

case class Filled(value: Int) extends Field {
  override def isFilled: Boolean = true
  override def isImpossible: Boolean = false
  override def toString: String = value.toString
}
case class Free(options: Set[Int]) extends Field {
  override def isFilled: Boolean = false
  override def isImpossible: Boolean = options.isEmpty
  override def toString: String = options.toString()
}
