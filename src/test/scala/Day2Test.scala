import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Day2Test extends AnyFlatSpec with should.Matchers {
  val input =
    """A Y
      |B X
      |C Z""".stripMargin

  "ans" should "work" in {
    Day2.ans(input) should be(8 + 1 + 6)

  }

  "ans2" should "work" in {
    Day2.ans2(input) should be(4 + 1 + 7)
  }
}
