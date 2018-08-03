import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class SolverSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "A sequence of fields" should "be updated by removing filled numbers from free fields" in {
    val fields = Seq(Free(Set(1, 2)), Free(Set(1, 3, 5)), Filled(1))
    val updatedFields = Solver.updateFields(fields)
    val expected = Seq(Filled(2), Free(Set(3, 5)), Filled(1))
    updatedFields shouldBe expected
  }

}
