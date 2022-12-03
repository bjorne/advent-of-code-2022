object Day3 extends Shared {

  object C {
    val a: Int = 'a'.toInt
    val z: Int = 'z'.toInt
    val A: Int = 'A'.toInt
    val a2z: Range.Inclusive = a to z
  }

  def priority(c: Char) = c.toInt match {
    case c if C.a2z contains c => c - C.a + 1
    case _                     => c - C.A + 27
  }

  def ans(input: String) =
    input
      .split("\n")
      .map(_.toCharArray.map(priority))
      .map { items =>
        items.splitAt(items.length / 2) match {
          case (left, right) => (left.toSet & right.toSet).head
        }
      }
      .sum

  def ans2(input: String) = input
    .split("\n")
    .map(_.toCharArray.map(priority))
    .grouped(3)
    .map(_.map(_.toSet).reduce(_ & _).head)
    .sum

  println(ans(input))
  println(ans2(input))
}
