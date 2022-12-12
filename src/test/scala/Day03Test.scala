import org.scalatest.matchers.should

class Day03Test extends DaySpec(Day03) {
  dayDone(7763, 2569)

  val input =
    """vJrwpWtwJgWrhcsFMMfFFhFp
      |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
      |PmmdzqPrVvPwwTWBwg
      |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
      |ttgJtRGJQctTZtZT
      |CrZsJsPPZsGzwwsLwLmpwMDw""".stripMargin

  "ans" should "work" in {
    today.ans(input) should be(16 + 38 + 42 + 22 + 20 + 19)

  }

  "ans2" should "work" in {
    today.ans2(input) should be(18 + 52)
  }
}
