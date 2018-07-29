import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class FieldSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "A field created from digit 0" should "be a free field with values from 1 to 9" in {
    val field = Field.fromChar('0')
    val expected = Free((1 to 9).toSet)
    field shouldBe expected
  }

  "A field created from digit 5" should "be a filled field with digit 5" in {
    val field = Field.fromChar('5')
    val expected = Filled(5)
    field shouldBe expected
  }

  "Field.fromChar" should "throw an IllegalArgumentException when fed a non-digit character" in {
    forAll {c: Char =>
      whenever (!Character.isDigit(c)) { an [IllegalArgumentException] should be thrownBy Field.fromChar(c)}
    }
  }

  "A free field" should "return false when asked if it is filled" in {
    val field = Free(Set(1, 5, 7))
    field.isFilled shouldBe false
  }

  "A filled field" should "return true when asked if it is filled" in {
    val field = Filled(7)
    field.isFilled shouldBe true
  }

  "A free field" should "be considered impossible if and only if it has no options" in {
    forAll { s: Set[Int] =>
      Free(s).isImpossible shouldBe s.isEmpty
    }
  }

  "A filled field" should "return false when asked if it is impossible" in {
    forAll { n: Int =>
      Filled(n).isImpossible shouldBe false
    }
  }
}