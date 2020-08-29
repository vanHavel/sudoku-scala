package sudoku

import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

trait testPuzzles {
  // sudokus with solutins taken from http://lipas.uwasa.fi/~timan/sudoku/
  val puzzle1: Puzzle = Puzzle.fromString("002000500010705020400090007049000730801030409036000210200080004080902060007000800")
  val solution1: Puzzle = Puzzle.fromString("972863541618745923453291687549128736821637459736459218295386174184972365367514892")
  val puzzle2: Puzzle = Puzzle.fromString("000000000079050180800000007007306800450708096003502700700000005016030420000000000")
  val solution2: Puzzle = Puzzle.fromString("345871269279653184861429537197346852452718396683592741738264915516937428924185673")
  val puzzle3: Puzzle = Puzzle.fromString("000003017015009008060000000100007000009000200000500004000000020500600340340200000")
  val solution3: Puzzle = Puzzle.fromString("294863517715429638863751492152947863479386251638512974986134725521678349347295186")
}
class SolverSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks with testPuzzles {

  "A sequence of fields" should "be updated by removing filled numbers from free fields" in {
    val fields = Seq(Free(Set(1, 2)), Free(Set(1, 3, 5)), Filled(1))
    val updatedFields = Solver.updateFields(fields)
    val expected = Seq(Free(Set(2)), Free(Set(3, 5)), Filled(1))
    updatedFields shouldBe expected
  }

  "Solver.solve" should "solve test puzzle 1 correctly" in {
    Solver.solve(puzzle1) shouldBe Some(solution1)
  }

  "Solver.solve" should "solve test puzzle 2 correctly" in {
    Solver.solve(puzzle2) shouldBe Some(solution2)
  }

  "Solver.solve" should "solve test puzzle 3 correctly" in {
    Solver.solve(puzzle3) shouldBe Some(solution3)
  }

}
