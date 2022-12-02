import scala.io.{BufferedSource, Source}

trait Shared extends App {
  def input: String = args match {
    case Array(filename) => Source.fromFile(filename).getLines.mkString("\n")
    case Array()         => Source.fromResource(s"$today.txt").getLines.mkString("\n")
    case _ =>
      throw new IllegalArgumentException(
        s"Usage: $today [filename]"
      )
  }

  def today: String = this.getClass.getName.replace("$", "")
}
