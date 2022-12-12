import org.scalatest.matchers.should

class Day06Test extends DaySpec(Day06) {
  dayDone(1816, 2625)
  val input =
    """mjqjpqmgbljsphdztnvjfqwrcgsmlb""".stripMargin

  "ans" should "work" in {
    Day06.ans(input) should be(7)
  }

  "ans2" should "work" in {
    Day06.ans2(input) should be(19)
  }
}
