import org.scalatest.matchers.should

class Day01Test extends DaySpec(Day01) {
  dayDone(72718, 213089)

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
    Day01.ans(input) should be(24000)
  }

  "ans2" should "work" in {
    Day01.ans2(input) should be(24000 + 11000 + 10000)
  }
}
