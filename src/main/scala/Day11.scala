import lib.Point

object Day11 extends Shared {
  val rounds = 20
  val items = """Starting items: (.+)""".r.unanchored
  val operation = """Operation: new = old (.) (.+)?""".r.unanchored
  val test = """Test: divisible by (.+)""".r.unanchored
  val testTrue = """If true: throw to monkey (.+)""".r.unanchored
  val testFalse = """If false: throw to monkey (.+)""".r.unanchored

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

  def compute(input: String, rounds: Int, divideBy: Int) = {
    val monkeys = input.split("\n\n").map { str =>
      Monkey(
        str match {
          case items(items: String) => items.split(", ").map(_.toLong)
        },
        str match {
          case operation(op: String, operand: String) => Operation(op, operand)
        },
        str match { case test(divisor: String) => divisor.toInt },
        str match { case testTrue(target: String) => target.toInt },
        str match { case testFalse(target: String) => target.toInt }
      )
    }
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

  println(ans(input))
  println(ans2(input))
}
