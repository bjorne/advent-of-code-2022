import org.scalatest.matchers.should

class Day14Test extends DaySpec(Day14) {
  dayDone(799, 29076)

  val input = """498,4 -> 498,6 -> 496,6
                |503,4 -> 502,4 -> 502,9 -> 494,9""".stripMargin

  "ans" should "work" in {
    today.ans(input) should be(24)
  }

  "ans2" should "work" in {
    today.ans2(input) should be(93)
  }
}
