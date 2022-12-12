object Day02 extends Shared {
  lazy val Pair = """^([A-C]) ([X-Z])$""".r

  enum Hand {
    case Rock, Paper, Scissors

    def score = ordinal + 1

    def losesTo = Hand.fromOrdinal((ordinal + 1) % 3)
    def winsOver = Hand.fromOrdinal((3 + ordinal - 1) % 3)
  }

  import Hand._
  lazy val mapping = Map(
    "A" -> Rock,
    "B" -> Paper,
    "C" -> Scissors,
    "X" -> Rock,
    "Y" -> Paper,
    "Z" -> Scissors
  )

  enum Outcome {
    case Draw, Win, Lose

    def score = this match {
      case Lose => 0
      case Draw => 3
      case Win  => 6
    }
  }

  import Outcome._

  val outcomeMapping = Map(
    "X" -> Lose,
    "Y" -> Draw,
    "Z" -> Win
  )

  def outcome(theirs: Hand, ours: Hand): Outcome = (theirs, ours) match {
    case (theirs, ours) if ours.winsOver == theirs => Win
    // or
    // case (Rock, Paper)                             => 6
    // case (Paper, Scissors)                         => 6
    // case (Scissors, Rock)                          => 6
    case (theirs, ours) if theirs == ours => Draw
    case _                                => Lose
  }

  override def ans(input: String) = input
    .split("\n")
    .toList
    .map { case Pair(a, b) =>
      val theirs = mapping(a)
      val ours = mapping(b)
      ours.score + outcome(theirs, ours).score
    }
    .sum

  override def ans2(input: String) = input
    .split("\n")
    .toList
    .map { case Pair(a, b) =>
      val theirs = mapping(a)
      val knownOutcome = outcomeMapping(b)
      val ours = knownOutcome match {
        case Draw => theirs
        case Win  => theirs.losesTo
        case Lose => theirs.winsOver
      }
      ours.score + knownOutcome.score
    }
    .sum

//  println(ans(input))
//  println(ans2(input))
}
