import org.scalatest.matchers.should

class Day08Test extends DaySpec(Day08) {
  dayDone(1776, 234416)
  val input =
    """30373
      |25512
      |65332
      |33549
      |35390""".stripMargin

  "ans" should "work" in {
    Day08.ans(input) should be(16 + 5)
  }

  "ans2" should "work" in {
    Day08.ans2(input) should be(2 * 2 * 1 * 2)
  }
}
