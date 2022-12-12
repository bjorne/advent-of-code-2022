import org.scalatest.matchers.should

class Day04Test extends DaySpec(Day04) {
  dayDone(560, 839)

  val input =
    """2-4,6-8
      |2-3,4-5
      |5-7,7-9
      |2-8,3-7
      |6-6,4-6
      |2-6,4-8""".stripMargin

  "ans" should "work" in {
    today.ans(input) should be(2)

  }

  "ans2" should "work" in {
    today.ans2(input) should be(4)
  }
}
