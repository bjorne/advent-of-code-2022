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
  def drawPoints(points: Map[Point, Char]): String = {
    val min =
      points.keys.reduce((p1, p2) => Point(p1.x.min(p2.x), p1.y.min(p2.y)))
    val max =
      points.keys.reduce((p1, p2) => Point(p1.x.max(p2.x), p1.y.max(p2.y)))
    println(s"$min $max")
    val chars = for {
      x <- (min.x - 1) to (max.x + 1)

    } yield {
      for {
        y <- (min.y - 1) to (max.y + 1)
        p = Point(x, y)
      } yield points.getOrElse(p, '.')
    }
    chars.transpose.map(_.mkString("")).reverse.mkString("\n")
  }
}
