import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Day1Test extends AnyFlatSpec with should.Matchers {
  val input =
    """1000
      |2000
      |3000
      |
      |4000
      |
      |5000
      |6000
      |
      |7000
      |8000
      |9000
      |
      |10000
      |""".stripMargin

  "ans" should "work" in {
    Day1.ans(input) should be(24000)
  }

  "ans2" should "work" in {
    Day1.ans2(input) should be(24000 + 11000 + 10000)
  }
}
