import scala.io.Source

object Day1 extends Shared {
  def ans2(input: String) = input
    .split("\n\n")
    .map(_.split("\n").map(_.toInt).sum)
    .sorted
    .reverse
    .take(3)
    .sum

  def ans(input: String) = input
    .split("\n\n")
    .map(_.split("\n").map(_.toInt).sum)
    .max

  println(ans(input))
  println(ans2(input))
}
