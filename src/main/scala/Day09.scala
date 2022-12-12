import lib.Point

object Day09 extends Shared {
  val directions = Map(
    "R" -> Point(1, 0),
    "U" -> Point(0, 1),
    "L" -> Point(-1, 0),
    "D" -> Point(0, -1)
  )

  def moveTail(head: Point, tail: Point): Point = {
    val move = head - tail match {
      case Point(x, y) if x.abs > 1 => Point(x.sign, y.sign)
      case Point(x, y) if y.abs > 1 => Point(x.sign, y.sign)
      case _                        => Point()
    }
    tail + move
  }

  case class State(
      head: Point = Point(),
      tail: Point = Point(),
      visited: Set[Point] = Set.empty
  )

  override def ans(input: String) =
    input
      .split("\n")
      .flatMap { case s"$direction $value" =>
        List.fill(value.toInt)(directions(direction))
      }
      .foldLeft(State()) { (state, move) =>
        val head = state.head + move
        val tail = moveTail(head, state.tail)
        State(head, tail, state.visited + tail)
      }
      .visited
      .size

  case class MultiState(
      head: Point = Point(),
      tail: List[Point] = List.fill(9)(Point()),
      visited: Set[Point] = Set.empty
  )

  override def ans2(input: String) = input
    .split("\n")
    .flatMap { case s"$direction $value" =>
      List.fill(value.toInt)(directions(direction))
    }
    .foldLeft(MultiState()) { (state, move) =>
      val head = state.head + move
      val tail = state.tail.scanLeft(head)(moveTail).drop(1)
      MultiState(head, tail, state.visited + tail.last)
    }
    .visited
    .size

  // println(ans(input))
  // println(ans2(input))
}
