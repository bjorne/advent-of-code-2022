import org.scalatest.matchers.should

class Day05Test extends DaySpec(Day05) {
  dayDone("GRTSWNJHH", "QLFQDBBHM")
  val input =
    """    [D]    
      |[N] [C]    
      |[Z] [M] [P]
      | 1   2   3 
      |
      |move 1 from 2 to 1
      |move 3 from 1 to 3
      |move 2 from 2 to 1
      |move 1 from 1 to 2""".stripMargin

  "ans" should "work" in {
    Day05.ans(input) should be("CMZ")
  }

  "ans2" should "work" in {
    Day05.ans2(input) should be("MCD")
  }
}
