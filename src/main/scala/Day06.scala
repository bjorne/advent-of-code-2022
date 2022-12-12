import scala.collection.mutable
import scala.util.Try

object Day06 extends Shared {

  override def ans(input: String) =
    4 + input.sliding(4).indexWhere(_.toSet.size == 4)

  override def ans2(input: String) =
    14 + input.sliding(14).indexWhere(_.toSet.size == 14)

  // println(ans(input))
  // println(ans2(input))
}
