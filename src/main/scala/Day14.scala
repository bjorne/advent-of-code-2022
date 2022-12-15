import jdk.internal.jimage.decompressor.SignatureParser.ParseResult
import lib.{Grid, Point}

import scala.util.parsing.combinator.RegexParsers

object Day14 extends Shared {
  val Source = Point(500, 0)
  def run(input: String)(isDone: (Point, Grid[Char]) => Boolean): Int = {
    val rock = input
      .split("\n")
      .flatMap(
        _.split(" -> ")
          .map { case s"$x,$y" => Point(x.toInt, y.toInt) }
          .sliding(2)
          .flatMap { case Array(a, b) => a to b }
      )
    val grid = Grid.fill(rock, '#')
    val floorY = rock.map(_.y).max + 2
    def floorOr(grid: Grid[Char])(p: Point): Boolean =
      Some(p).filter(_.y == floorY).map(_ => '#').orElse(grid(p)).isEmpty

    LazyList
      .unfold(grid) { grid =>
        val f = floorOr(grid)(_)
        val grain = LazyList
          .unfold(Source) { p =>
            Seq(p.up, p.up.left, p.up.right)
              .find(f)
              .map(p => (p, p))
          }
          .lastOption
          .getOrElse(Source)
        grain match {
          case p if isDone(p, grid) => None
          case p                    => Some(((), grid.updated(p, 'o')))
        }
      }
      .length
  }

  override def ans(input: String) = run(input) { (p, grid) =>
    p.y >= grid.points.map(_._1.y).max
  }

  override def ans2(input: String) = run(input) { (p, grid) =>
    p == Source && grid(p).nonEmpty
  }
}
