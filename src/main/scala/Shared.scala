import scala.io.{BufferedSource, Source}

trait Shared extends App {
  def input: String = args match {
    case Array(filename) => Source.fromFile(filename).getLines.mkString("\n")
    case _ => Source.fromResource(s"$today.txt").getLines.mkString("\n")
  }

  def today: String = this.getClass.getName.replace("$", "")

  def ans(input: String): Any
  def ans2(input: String): Any
}
