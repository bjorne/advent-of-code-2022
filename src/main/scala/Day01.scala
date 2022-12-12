import scala.io.Source

object Day01 extends Shared {
  override def ans2(input: String) = input
    .split("\n\n")
    .map(_.split("\n").map(_.toInt).sum)
    .sorted
    .reverse
    .take(3)
    .sum

  override def ans(input: String) = input
    .split("\n\n")
    .map(_.split("\n").map(_.toInt).sum)
    .max

//  println(ans(input))
//  println(ans2(input))
}
