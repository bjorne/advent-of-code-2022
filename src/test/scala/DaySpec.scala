import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.reflect.ClassTag

trait DaySpec(_today: Shared) extends AnyFlatSpec with should.Matchers {

  def today = _today

  def dayDone(part1: Any = null, part2: Any = null) = {
    val name = today.getClass.getName.replace("$", "")
    if (part1 != null) {
      s"$name.ans" should s"give $part1" in {
        today.ans(today.input) should be(part1)
      }
    }
    if (part2 != null) {
      s"$name.ans2" should s"give $part2" in {
        today.ans2(today.input) should be(part2)
      }
    }

  }
}
