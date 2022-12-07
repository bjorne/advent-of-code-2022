import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try

object Day7 extends Shared {

  sealed trait LsEntry
  case class Dir(name: String) extends LsEntry
  object Dir {
    val regex = """^dir (.+)""".r
  }
  case class File(name: String, size: Int) extends LsEntry
  object File {
    val regex = """^(\d+) (.+)""".r
  }

  sealed trait Command
  case class Cd(to: String) extends Command
  object Cd {
    val regex = """^cd (.+)""".r
  }
  case class Ls(entries: Seq[LsEntry]) extends Command
  object Ls {
    val regex = """(?s)^ls\n(.*)""".r

    def parseOutput(output: String): Seq[LsEntry] =
      output.split("\n").map {
        case Dir.regex(name)        => Dir(name)
        case File.regex(size, name) => File(name, size.toInt)
      }
  }

  case class Interpreter(cwd: String, sizes: Map[String, Int]) {
    def cd(to: String): Interpreter = to match {
      case "/"  => copy(cwd = "")
      case ".." => copy(cwd = cwd.split("/").dropRight(1).mkString("/"))
      case dir  => copy(cwd = s"$cwd/$dir")
    }
  }

  case class TreeNode(size: Int, children: Seq[TreeNode]) {
    lazy val totalSize: Int = size + children.map(_.totalSize).sum

    def collect(cond: TreeNode => Boolean): List[TreeNode] = Option
      .when(cond(this))(this)
      .toList ++ children.flatMap(_.collect(cond))

  }
  def buildTree(
      from: String,
      sizes: Map[String, Int]
  ): TreeNode = {
    TreeNode(
      sizes(from),
      sizes.keys
        .filter(_.matches(s"$from/[^/]+"))
        .map(buildTree(_, sizes))
        .toSeq
    )
  }

  private def extractTree(input: String) = {
    val transcript =
      input.split("""\$ """).filter(_.nonEmpty).map(_.stripLineEnd).map {
        case Cd.regex(to: String)     => Cd(to)
        case Ls.regex(output: String) => Ls(Ls.parseOutput(output))
      }
    val sizes = transcript
      .foldLeft(Interpreter("/", Map.empty)) { (interpreter, command) =>
        command match {
          case Cd(to) => interpreter.cd(to)
          case Ls(entries) =>
            val size = entries.collect { case File(_, size) => size }.sum
            interpreter
              .copy(sizes = interpreter.sizes.updated(interpreter.cwd, size))
        }
      }
      .sizes
    buildTree("", sizes)
  }

  def ans(input: String) =
    extractTree(input).collect(_.totalSize < 100_000).map(_.totalSize).sum

  val DISK_SIZE = 70_000_000
  val MIN_REQUIRED_SPACE = 30_000_000

  def ans2(input: String) = {
    val tree: TreeNode = extractTree(input)
    val freeSpace = DISK_SIZE - tree.totalSize
    val spaceToFree = MIN_REQUIRED_SPACE - freeSpace
    tree.collect(_.totalSize > spaceToFree).map(_.totalSize).min
  }

  println(ans(input))
  println(ans2(input))
}
