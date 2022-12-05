object Day4 extends Shared {

  implicit class SeqPair[T](pair: List[T]) {
    def pairApply[U](fn: (a: T, b: T) => U): U = pair match {
      case a :: b :: Nil => fn(a, b)
      case _             => throw new Error(s"Gahhh! Not 2 elements: $pair")
    }
  }

  private def parseInput(input: String) = {
    input
      .split("\n")
      .map(
        _.split(',').toList
          .map(_.split('-').map(_.toInt).toList)
          .map(_.pairApply(Range.inclusive))
      )
  }

  def ans(input: String) =
    parseInput(input)
      .count(_.pairApply { (l, r) =>
        (l containsSlice r) || (r containsSlice l)
      })

  def ans2(input: String) = parseInput(input)
    .count(_.pairApply { (l, r) =>
      (l contains r.start) || (l contains r.last)
      || (r contains l.start) || (r contains l.last)
    })

  println(ans(input))
  println(ans2(input))
}
