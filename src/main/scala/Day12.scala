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

  val BIG_NUMBER_BUT_FAR_FROM_INT_OVERFLOW = 100_000

  private def findDistances(
      points: Map[Point, Int],
      from: Point
  ): Map[Point, Int] =
    Iterator
      .iterate(
        (
          points.keys.toSet,
          Map(from -> 0).withDefault(_ => BIG_NUMBER_BUT_FAR_FROM_INT_OVERFLOW)
        )
      ) { case (unvisited, distances) =>
        val current = unvisited.minBy(distances)
        Seq(UP, DOWN, LEFT, RIGHT)
          .map(current + _)
          .filter(points.contains)
          .filter(p => points(p) >= points(current) - 1)
          .foldLeft((unvisited - current, distances)) { (pair, neighbor) =>
            pair match {
              case (unvisited, distances) =>
                val alt = distances(current) + 1
                if (alt < distances(neighbor))
                  (unvisited, distances.updated(neighbor, alt))
                else
                  (unvisited, distances)
            }
          }
      }
      .dropWhile(_._1.nonEmpty)
      .take(1)
      .next()
      ._2

  override def ans(input: String) = {
    val map = parseInput(input)
    val distances = findDistances(map.points, map.end.get)
    distances(map.start.get)
  }

  override def ans2(input: String) = {
    val map = parseInput(input)
    val distances = findDistances(map.points, map.end.get)
    map.points.collect {
      case (p, h) if h == 0 => distances(p)
    }.min
  }
}
