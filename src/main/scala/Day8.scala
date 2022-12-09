import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try

object Day8 extends Shared {

  implicit class ArrayOps(a: Array[Int]) {
    def lastIndexWhereOr(fn: Int => Boolean)(or: => Int): Int =
      a.lastIndexWhere(fn) match {
        case -1 => or
        case x  => x
      }

    def indexWhereOr(fn: Int => Boolean)(or: => Int): Int =
      a.indexWhere(fn) match {
        case -1 => or
        case x  => x
      }
  }

  private def parseInput(input: String) =
    input.split("\n").map(_.split("").map(_.toInt))

  def ans(input: String) = {
    val grid = parseInput(input)
    val rows = grid.size
    val cols = grid.head.size

    val visible = for {
      r <- 1 until rows - 1
      c <- 1 until cols - 1
      v = grid(r)(c)
      vl = grid(r).indexWhere(_ >= v) == c
      vr = grid(r).reverse.indexWhere(_ >= v) == (cols - 1 - c)
      col = grid.map(_(c))
      vt = col.indexWhere(_ >= v) == r
      vb = col.reverse.indexWhere(_ >= v) == (rows - 1 - r)
      if vl || vr || vt || vb
    } yield true

    2 * rows + 2 * cols - 4 + visible.size
  }

  def ans2(input: String) = {
    val grid = parseInput(input)
    val rows = grid.size
    val cols = grid.head.size

    val scores = for {
      r <- 1 until rows - 1
      c <- 1 until cols - 1
      row = grid(r)
      col = grid.map(_(c))
      v = row(c)
      vt = (r - col.take(r).lastIndexWhereOr(_ >= v)(0)).max(1)
      vl = (c - row.take(c).lastIndexWhereOr(_ >= v)(0)).max(1)
      vr = row.drop(c + 1).indexWhereOr(_ >= v)(cols - c) + 1
      vb = col.drop(r + 1).indexWhereOr(_ >= v)(rows - r - 1)
    } yield vl * vt * vr * vb

    scores.max
  }

  println(ans(input))
  println(ans2(input))
}
