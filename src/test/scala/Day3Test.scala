import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Day3Test extends AnyFlatSpec with should.Matchers {
  val input =
    """vJrwpWtwJgWrhcsFMMfFFhFp
      |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
      |PmmdzqPrVvPwwTWBwg
      |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
      |ttgJtRGJQctTZtZT
      |CrZsJsPPZsGzwwsLwLmpwMDw""".stripMargin

  "ans" should "work" in {
    Day3.ans(input) should be(16 + 38 + 42 + 22 + 20 + 19)

  }

  "ans2" should "work" in {
    Day3.ans2(input) should be(18 + 52)
  }
}
