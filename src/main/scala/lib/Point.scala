package lib

import scala.annotation.targetName

case class Point(x: Int = 0, y: Int = 0) {
  @targetName("plus")
  def +(other: Point): Point = Point(x + other.x, y + other.y)

  @targetName("minus")
  def -(other: Point): Point = Point(x - other.x, y - other.y)

  def to(o: Point): Iterator[Point] = o match {
    case p if p == this => Iterator.apply(this)
    case Point(ox, oy) if ox == x =>
      (y to oy by (oy - y).sign).iterator.map(Point(x, _))
    case Point(ox, oy) if oy == y =>
      (x to ox by (ox - x).sign).iterator.map(Point(_, y))
    case _ => throw new IllegalArgumentException("Points not in line")
  }

  def up: Point = this + Point.up
  def down: Point = this + Point.down
  def left: Point = this + Point.left
  def right: Point = this + Point.right

  def taxicabDistance: Int = x.abs + y.abs
  def abs: Point = Point(x.abs, y.abs)

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

  val origin = Point(0, 0)
  val up = Point(0, 1)
  val down = Point(0, -1)
  val left = Point(-1, 0)
  val right = Point(1, 0)
}
