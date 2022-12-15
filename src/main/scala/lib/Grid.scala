package lib

case class Grid[T](points: Map[Point, T]) {
  def apply(p: Point): Option[T] = points.get(p)

  def draw(fn: T => Char): String =
    Point.drawPoints(points.map(pair => (pair._1, fn(pair._2))))

  def updated(p: Point, v: T): Grid[T] =
    copy(points = points.updated(p, v))
}

object Grid {
  def fill[T](points: Seq[Point], v: T): Grid[T] = Grid(
    points.map(_ -> v).toMap
  )
}
