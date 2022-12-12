import Day12.DOWN
import jdk.internal.jimage.decompressor.SignatureParser.ParseResult
import lib.Point

import scala.util.parsing.combinator.RegexParsers

object Day12 extends Shared {
  case class MapState(
      points: Map[Point, Int] = Map.empty,
      start: Option[Point] = None,
      end: Option[Point] = None
  )

  val UP = Point(0, 1)
  val DOWN = Point(0, -1)
  val LEFT = Point(-1, 0)
  val RIGHT = Point(1, 0)

  private def parseInput(input: String) = {
    input
      .split("\n")
      .zipWithIndex
      .foldLeft(
        MapState()
      ) { (state, pair) =>
        pair match {
          case (line, row) =>
            line.toCharArray.zipWithIndex.foldLeft(state) { (state, pair) =>
              pair match {
                case (char, col) =>
                  val p = Point(col, row)
                  char match {
                    case 'S' =>
                      state.copy(
                        start = Some(p),
                        points = state.points.updated(p, 0)
                      )
                    case 'E' =>
                      state.copy(
                        end = Some(p),
                        points = state.points.updated(p, 'z'.toInt - 'a'.toInt)
                      )
                    case c =>
                      state.copy(points =
                        state.points.updated(p, c.toInt - 'a'.toInt)
                      )
                  }
              }
            }
        }
      }
  }

  def countSteps(points: Map[Point, Int], from: Point, to: Set[Point]): Int =
    LazyList
      .unfold((0, Seq(from))) { case (steps, next) =>
        Option.when((to & next.toSet).isEmpty) {
          val neighbors = next.flatMap { p =>
            Seq(UP, DOWN, LEFT, RIGHT)
              .map(p + _)
              .filter(n => points.get(n).exists(_ >= points(p) - 1))
          }.distinct
          (steps + 1, (steps + 1, neighbors))
        }
      }
      .last

  override def ans(input: String) = {
    val map = parseInput(input)
    countSteps(map.points, map.end.get, Set(map.start.get))
  }

  override def ans2(input: String) = {
    val map = parseInput(input)
    countSteps(
      map.points,
      map.end.get,
      map.points.collect { case (p, h) if h == 0 => p }.toSet
    )
  }
}
