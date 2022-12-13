import org.scalatest.matchers.should

class Day13Test extends DaySpec(Day13) {
  dayDone(5003, 20280)

  val input = """[1,1,3,1,1]
                |[1,1,5,1,1]
                |
                |[[1],[2,3,4]]
                |[[1],4]
                |
                |[9]
                |[[8,7,6]]
                |
                |[[4,4],4,4]
                |[[4,4],4,4,4]
                |
                |[7,7,7,7]
                |[7,7,7]
                |
                |[]
                |[3]
                |
                |[[[]]]
                |[[]]
                |
                |[1,[2,[3,[4,[5,6,7]]]],8,9]
                |[1,[2,[3,[4,[5,6,0]]]],8,9]""".stripMargin

  "ans" should "work" in {
    today.ans(input) should be(1 + 2 + 4 + 6)
  }

  "ans2" should "work" in {
    today.ans2(input) should be(10 * 14)
  }
}
