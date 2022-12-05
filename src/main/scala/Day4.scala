object Day4 extends Shared {

  val line = """(\d+)-(\d+),(\d+)-(\d+)""".r

  private def parseInput(input: String) = {
    input
      .split("\n")
      .map { case line(a, b, c, d) => (a.toInt to b.toInt, c.toInt to d.toInt) }
  }

  def ans(input: String) =
    parseInput(input)
      .count { case (l, r) => (l containsSlice r) || (r containsSlice l) }

  def ans2(input: String) = parseInput(input)
    .count { case (l, r) =>
      (l contains r.start) || (l contains r.last)
      || (r contains l.start) || (r contains l.last)
    }

  println(ans(input))
  println(ans2(input))
}
