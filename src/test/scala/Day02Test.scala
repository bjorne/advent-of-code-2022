import org.scalatest.matchers.should

class Day02Test extends DaySpec(Day02) {
  dayDone(11386, 13600)

  val input =
    """A Y
      |B X
      |C Z""".stripMargin

  "ans" should "work" in {
    today.ans(input) should be(8 + 1 + 6)

  }

  "ans2" should "work" in {
    today.ans2(input) should be(4 + 1 + 7)
  }
}
