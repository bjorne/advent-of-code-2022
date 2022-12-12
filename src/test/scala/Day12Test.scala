import org.scalatest.matchers.should

class Day12Test extends DaySpec(Day12) {
  dayDone(534, 525)

  val input = """Sabqponm
                |abcryxxl
                |accszExk
                |acctuvwj
                |abdefghi""".stripMargin

  "ans" should "work" in {
    today.ans(input) should be(31)
  }

  "ans2" should "work" in {
    today.ans2(input) should be(29)
  }
}
