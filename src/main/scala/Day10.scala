import lib.Point

object Day10 extends Shared {
  val checkpoints = List(20, 60, 100, 140, 180, 220)
  val width = 40
  val height = 6

  private def run(input: String) = {
    input
      .split("\n")
      .flatMap {
        case s"addx $value" => List(0, value.toInt)
        case s"noop"        => List(0)
      }
      .scanLeft(1) { (instr, acc) => acc + instr }
      .zipWithIndex
  }

  override def ans(input: String) = run(input).collect {
    case (v, index) if checkpoints.contains(index + 1) => v * (index + 1)
  }.sum

  override def ans2(input: String) = {
    val pixels = run(input)
      .foldLeft(Set.empty[Point]) { (pixels, pair) =>
        pair match {

          case (value, index) =>
            val add = ((value - 1) to (value + 1))
              .find(_ == index % 40)
              .map { v =>
                val withOffset = (index % 40 - v) + index
                Point(withOffset % 40, -withOffset / 40)
              }
              .toList
            pixels ++ add
        }
      }
    Point.drawPoints(pixels.map(p => p -> '#').toMap, padding = 0)
  }

  // println(ans(input))
  // println(ans2(input))
}
