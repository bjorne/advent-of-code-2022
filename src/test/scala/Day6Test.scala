import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Day6Test extends AnyFlatSpec with should.Matchers {
  val input =
    """mjqjpqmgbljsphdztnvjfqwrcgsmlb""".stripMargin

  "ans" should "work" in {
    Day6.ans(input) should be(7)
  }

  "ans2" should "work" in {
    Day6.ans2(input) should be(19)
  }
}
