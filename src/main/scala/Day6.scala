import scala.collection.mutable
import scala.util.Try

object Day6 extends Shared {

  def ans(input: String) =
    4 + input.sliding(4).indexWhere(_.toCharArray.toSet.size == 4)

  def ans2(input: String) =
    14 + input.sliding(14).indexWhere(_.toCharArray.toSet.size == 14)

  println(ans(input))
  println(ans2(input))
}
