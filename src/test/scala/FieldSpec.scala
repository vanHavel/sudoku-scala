import org.scalatest._

class FieldSpec extends FlatSpec with Matchers {

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

  "A free field" should "return false when asked if it is filled" in {
    val field = Free(Set(1, 5, 7))
    field.isFilled shouldBe false
  }

  "A filled field" should "return true when asked if it is filled" in {
    val field = Filled(7)
    field.isFilled shouldBe true
  }

  "A free field with no options" should "return true when asked if it is impossible" in {
    val field = Free(Set())
    field.isImpossible shouldBe true
  }

  "A free field with options" should "return false when asked if it is impossible" in {
    val field = Free(Set(2, 6))
    field.isImpossible shouldBe false
  }

  "A filled field" should "return false when asked if it is impossible" in {
    val field = Filled(6)
    field.isImpossible shouldBe false
  }
}