import lib.Point
import org.scalatest.matchers.should

class Day15Test extends DaySpec(Day15) {
  dayDone(4560025)

  val input = """Sensor at x=2, y=18: closest beacon is at x=-2, y=15
                |Sensor at x=9, y=16: closest beacon is at x=10, y=16
                |Sensor at x=13, y=2: closest beacon is at x=15, y=3
                |Sensor at x=12, y=14: closest beacon is at x=10, y=16
                |Sensor at x=10, y=20: closest beacon is at x=10, y=16
                |Sensor at x=14, y=17: closest beacon is at x=10, y=16
                |Sensor at x=8, y=7: closest beacon is at x=2, y=10
                |Sensor at x=2, y=0: closest beacon is at x=2, y=10
                |Sensor at x=0, y=11: closest beacon is at x=2, y=10
                |Sensor at x=20, y=14: closest beacon is at x=25, y=17
                |Sensor at x=17, y=20: closest beacon is at x=21, y=22
                |Sensor at x=16, y=7: closest beacon is at x=15, y=3
                |Sensor at x=14, y=3: closest beacon is at x=15, y=3
                |Sensor at x=20, y=1: closest beacon is at x=15, y=3""".stripMargin

  "naive" should "work" in {
    pending
    Day15.naive(input, y = 10) should be(26)
  }

  "ansY" should "work" in {
    Day15.ansY(input, ySearch = 10) should be(26)
  }

  "ans2" should "work" in {
    pending
    Day15.ans2Y(input, 0 to 20) should be(1)
  }

  "Line#intersect" should "work" in {
    pending
    import Day15.Line

    Line(Point(-1, -1), Point(1, 1)).intersect(Line(Point(-1, 1), Point(1, -1))) should contain(
      Point.origin
    )
    Line(Point(-5, -5), Point(1, 1)).intersect(Line(Point(-2, 1), Point(0, -1))) should contain(
      Point.origin.left
    )
    Line(Point(-1, -1), Point(1, 1)).intersect(Line(Point(2, 1), Point(4, -1))) should contain(
      Point.origin.left
    )
  }
}
