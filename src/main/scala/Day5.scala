import scala.collection.mutable
import scala.util.Try

object Day5 extends Shared {

  lazy val parts = """(?s)(.+)\n\n(.+)""".r
  lazy val move = """move (\d+) from (\d+) to (\d+)""".r

  case class Move(count: Int, from: Int, to: Int)
  // case class State(piles: Seq[List[Char]], moves: Seq[Move])

  def parseInput(input: String) = {
    val (pilesIn, instructionsIn) = input.split("\n").span(_ != "")
    val numPiles =
      pilesIn.last.split(" ").filter(_.nonEmpty).map(_.toInt).max
    val piles = (0 until numPiles).map { pile =>
      (new mutable.Stack).addAll(
        pilesIn
          .dropRight(1)
          .map { line => Try(line(1 + 4 * pile)).getOrElse(' ') }
          .filterNot(_ == ' ')
      )
    }
    val instructions = instructionsIn.drop(1).map {
      case move(count: String, from: String, to: String) =>
        Move(count.toInt, from.toInt - 1, to.toInt - 1)
    }
    (piles, instructions)
  }

  def ans(input: String) = {
    val (piles, instructions) = parseInput(input)
    for {
      move <- instructions
      _ <- 1 to move.count
    } yield piles(move.to).push(piles(move.from).pop)
    piles.map(_.top).mkString("")
  }

  def ans2(input: String) = {
    val (piles, instructions) = parseInput(input)
    for {
      move <- instructions
    } yield {
      val popped = for (_ <- 1 to move.count) yield piles(move.from).pop
      piles(move.to).pushAll(popped.reverse)
    }
    piles.map(_.top).mkString("")
  }
  println(ans(input))
  println(ans2(input))
}
