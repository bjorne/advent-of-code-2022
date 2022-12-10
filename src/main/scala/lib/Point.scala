package lib

import scala.annotation.targetName

case class Point(x: Int = 0, y: Int = 0) {
  @targetName("plus")
  def +(other: Point): Point = Point(x + other.x, y + other.y)

  @targetName("minus")
  def -(other: Point): Point = Point(x - other.x, y - other.y)

  override def toString: String = s"($x, $y)"
}

object Point {
  def drawPoints(points: Map[Point, Char], padding: Int = 1): String = {
    val min =
      points.keys.reduce((p1, p2) => Point(p1.x.min(p2.x), p1.y.min(p2.y)))
    val max =
      points.keys.reduce((p1, p2) => Point(p1.x.max(p2.x), p1.y.max(p2.y)))
    val chars = for {
      x <- (min.x - padding) to (max.x + padding)

    } yield {
      for {
        y <- (min.y - padding) to (max.y + padding)
        p = Point(x, y)
      } yield points.getOrElse(p, '.')
    }
    chars.transpose.map(_.mkString("")).reverse.mkString("\n")
  }
}
