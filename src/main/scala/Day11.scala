import jdk.internal.jimage.decompressor.SignatureParser.ParseResult
import lib.Point

import scala.util.parsing.combinator.RegexParsers

object Day11 extends Shared {
  trait Operation {
    def eval(old: Long): Long
  }
  case class Plus(addend: Int) extends Operation {
    override def eval(old: Long): Long = old + addend
  }
  case class Times(factor: Int) extends Operation {
    override def eval(old: Long): Long = old * factor
  }
  case object Squared extends Operation {
    override def eval(old: Long): Long = old * old
  }

  object Operation {
    def apply(op: String, operand: String) = op match {
      case "*" if operand == "old" => Squared
      case "+"                     => Plus(operand.toInt)
      case "*"                     => Times(operand.toInt)
    }
  }

  case class Monkey(
      items: Seq[Long],
      operation: Operation,
      testDivisor: Int,
      trueTarget: Int,
      falseTarget: Int,
      inspections: Long = 0L
  )

  object MonkeyParser extends RegexParsers {
    def monkeyStart: Parser[String] = "Monkey .+:".r
    def number: Parser[Int] = """-?\d+""".r ^^ { _.toInt }
    def startingItems: Parser[List[Int]] =
      "Starting items:" ~> repsep(number, ",")
    def operator: Parser[String] = "*" | "+"
    def operand: Parser[String] =
      "old" | """\d+""".r
    def operation: Parser[Operation] =
      "Operation: new = old" ~> operator ~ operand ^^ {
        case operator ~ operand =>
          Operation(operator, operand)
      }
    def trueTarget: Parser[Int] = "If true: throw to monkey " ~> number
    def falseTarget: Parser[Int] = "If false: throw to monkey " ~> number
    def test: Parser[Int ~ Int ~ Int] =
      "Test: divisible by " ~> number ~ trueTarget ~ falseTarget
    def monkey: Parser[Monkey] =
      monkeyStart ~> startingItems ~ operation ~ test ^^ {
        case items ~ op ~ (divisor ~ trueTarget ~ falseTarget) =>
          Monkey(
            items.map(_.toLong),
            op,
            divisor,
            trueTarget,
            falseTarget
          )
      }
    def monkeys: Parser[List[Monkey]] =
      rep(monkey)
    def apply(input: String): List[Monkey] = parseAll(monkeys, input) match {
      case Success(result, _) => result
      case failure: NoSuccess => scala.sys.error(failure.msg)
    }
  }

  def compute(input: String, rounds: Int, divideBy: Int) = {
    val monkeys = MonkeyParser(input)
    val lcd = monkeys.map(_.testDivisor).product
    Iterator
      .iterate(monkeys) { (monkeys) =>
        monkeys.indices.foldLeft(monkeys) { (monkeys, index) =>
          val newMonkeys = monkeys(index).items
            .foldLeft(monkeys) { (monkeys, item) =>
              val current = monkeys(index)
              val inspectedValue = current.operation.eval(item) % lcd
              val inspectedItem = inspectedValue / divideBy
              val target =
                if (inspectedItem % current.testDivisor == 0) current.trueTarget
                else current.falseTarget
              monkeys
                .updated(
                  target,
                  monkeys(target)
                    .copy(items = monkeys(target).items.appended(inspectedItem))
                )
            }
          newMonkeys
            .updated(
              index,
              newMonkeys(index).copy(
                items = Seq.empty,
                inspections =
                  newMonkeys(index).inspections + monkeys(index).items.size
              )
            )
        }
      }
      .take(rounds + 1)
      .toSeq
      .last
      .map(_.inspections)
      .sorted
      .reverse
      .take(2)
      .product

  }

  def ans(input: String) = compute(input, 20, 3)
  def ans2(input: String) = compute(input, 10_000, 1)

  // println(ans(input))
  // println(ans2(input))
}
