import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class PuzzleSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "rowIndices" should "return range from 9 to 17 when called for row 1" in {
    val indices = Puzzle.rowIndices(1)
    val expected = 9 to 17
    indices shouldBe expected
  }

  "colIndices" should "return range from 2 to 74 by 9 when called for col 2" in {
    val indices = Puzzle.colIndices(2)
    val expected = 2 to 74 by 9
    indices shouldBe expected
  }

  "squareIndices" should "return indices of center square when called for square 4" in {
    val indices = Puzzle.squareIndices(4)
    val expected = Seq(30, 31, 32, 39, 40, 41, 48, 49, 50)
    indices shouldBe expected
  }
}
