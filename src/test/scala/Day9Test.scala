import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Day9Test extends AnyFlatSpec with should.Matchers {

  "ans" should "work" in {
    val input =
      """R 4
        |U 4
        |L 3
        |D 1
        |R 4
        |D 1
        |L 5
        |R 2""".stripMargin

    Day9.ans(input) should be(13)
  }

  "ans2" should "work" in {
    val input = """R 5
                  |U 8
                  |L 8
                  |D 3
                  |R 17
                  |D 10
                  |L 25
                  |U 20""".stripMargin
    Day9.ans2(input) should be(36)
  }
}
