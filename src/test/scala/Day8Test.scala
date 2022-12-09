import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Day8Test extends AnyFlatSpec with should.Matchers {
  val input =
    """30373
      |25512
      |65332
      |33549
      |35390""".stripMargin

  "ans" should "work" in {
    Day8.ans(input) should be(16 + 5)
  }

  "ans2" should "work" in {
    Day8.ans2(input) should be(2 * 2 * 1 * 2)
  }
}
