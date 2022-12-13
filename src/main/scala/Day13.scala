import jdk.internal.jimage.decompressor.SignatureParser.ParseResult
import lib.Point

import scala.util.parsing.combinator.RegexParsers

object Day13 extends Shared {
  sealed trait PacketList
  object PacketList {
    case class List(items: Seq[PacketList]) extends PacketList
    case class Number(value: Int) extends PacketList

    implicit object PacketListOrdering extends Ordering[PacketList] {
      def compare(a: PacketList, b: PacketList) = (a, b) match {
        case (a: PacketList.Number, b: PacketList.Number) =>
          a.value compare b.value
        case (a: PacketList.Number, b: PacketList.List) =>
          compare(PacketList.List(Seq(a)), b)
        case (a: PacketList.List, b: PacketList.Number) =>
          compare(a, PacketList.List(Seq(b)))
        case (a: PacketList.List, b: PacketList.List) =>
          a.items.zip(b.items).indexWhere(compare(_, _) != 0) match {
            case -1 => a.items.length compare b.items.length
            case x  => compare(a.items(x), b.items(x))
          }
      }
    }
  }

  object PacketParser extends RegexParsers {
    def number: Parser[PacketList] =
      """-?\d+""".r ^^ (v => PacketList.Number(v.toInt))

    def list: Parser[PacketList] =
      "[" ~> repsep(number | list, ",") <~ "]" ^^ PacketList.List.apply

    def apply(input: String): PacketList = parseAll(list, input) match {
      case Success(result, _) => result
      case failure: NoSuccess => scala.sys.error(failure.msg)
    }
  }

  override def ans(input: String) =
    input
      .split("\n\n")
      .map(_.split("\n").toSeq.map(PacketParser.apply))
      .zipWithIndex
      .collect { case (pair, index) if pair.sorted == pair => index + 1 }
      .sum

  val dividerPackets = Seq(
    PacketList.List(Seq(PacketList.List(Seq(PacketList.Number(2))))),
    PacketList.List(Seq(PacketList.List(Seq(PacketList.Number(6)))))
  )

  override def ans2(input: String) =
    input
      .split("\n\n")
      .flatMap(_.split("\n"))
      .toSeq
      .map(PacketParser.apply)
      .concat(dividerPackets)
      .sorted
      .zipWithIndex
      .collect {
        case (pl, index) if dividerPackets.contains(pl) => index + 1
      }
      .product

}
