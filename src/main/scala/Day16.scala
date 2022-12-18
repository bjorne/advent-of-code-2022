import jdk.internal.jimage.decompressor.SignatureParser.ParseResult
import lib.Point

import scala.collection.mutable
import scala.util.parsing.combinator.RegexParsers

object Day16 extends Shared {
  val timeLimit = 30

  case class Room(name: String, rate: Int, tunnels: Set[String])
  val line = """Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.+)""".r
  override def ans(input: String) = {
    val rooms = input
      .split("\n")
      .map { case line(valve, rate, tunnels) =>
        valve -> Room(valve, rate.toInt, tunnels.split(", ").toSet)
      }
      .toMap

    //    case class Iteration(current: Room, totalYield: Int, timeRemaining: Int, opened: Set[Room])
    //    case class Yield(room: Room, cumulativeYield: Int, timeSpent: Int)
    //
    //    def findHighestYield(
    //        next: Set[Room],
    //        it: Iteration,
    //        timeSpent: Int,
    //        visited: Set[Room]
    //    ): Option[Yield] = {
    //      next
    //        .flatMap { (room) =>
    //          val best = Seq
    //            .concat(
    //              Option.when(!it.opened.contains(room) && !visited.contains(room))(
    //                Yield(room, room.rate * (it.timeRemaining - timeSpent - 1), timeSpent + 1)
    //              ),
    //              Option
    //                .when(it.timeRemaining - (timeSpent + 1) > 1) {
    //                  findHighestYield(
    //                    room.tunnels.map(rooms.apply) -- visited,
    //                    it,
    //                    timeSpent + 1,
    //                    visited + room
    //                  )
    //                }
    //                .flatten
    //            )
    //            .maxByOption(_.cumulativeYield)
    //          println(
    //            s"$timeSpent -- Best from ${room.name} is ${best.map(_.room).getOrElse("N/A")} with ${best
    //                .map(_.cumulativeYield)
    //                .getOrElse("N/A")} and time spent ${best.map(_.timeSpent).getOrElse("N/A")}"
    //          )
    //          best
    //        }
    //        .maxByOption(_.cumulativeYield)
    //    }
    //
    //    LazyList
    //      .unfold(Iteration(rooms("AA"), 0, timeLimit, Set.empty)) { it =>
    //        Option
    //          .when(it.timeRemaining >= 2 || rooms.size > it.opened.size) {
    //            println(it)
    //            val nextOpt =
    //              findHighestYield(it.current.tunnels.map(rooms.apply), it, 1, Set.empty)
    //            nextOpt.map { next =>
    //              val newIt = it.copy(
    //                current = next.room,
    //                totalYield = it.totalYield + next.cumulativeYield,
    //                timeRemaining = it.timeRemaining - next.timeSpent,
    //                opened = it.opened + next.room
    //              )
    //              (newIt, newIt)
    //            }
    //          }
    //          .flatten
    //      }
    //      .tapEach(println)
    //      .last

    val cache = mutable.Map.empty[(Set[Room], Room, Int), Int]

    def findBest(opened: Set[Room], visited: Set[Room], current: Room, timeRemaining: Int): Int = {
      if (
        timeRemaining < 1
        || rooms.size == opened.size
        || visited.size == rooms.size
      ) return 0;
      cache.getOrElseUpdate(
        (opened, current, timeRemaining), {
          current.tunnels
            .map(rooms.apply)
            .map { room =>
              Seq
                .concat(
                  Option.when(!opened.contains(current)) {
                    current.rate * (timeRemaining - 1) + findBest(
                      opened + current,
                      Set(current),
                      room,
                      timeRemaining - 2
                    )
                  },
                  Option.when(!visited.contains(room)) {
                    findBest(opened, visited + room, room, timeRemaining - 1)
                  }
                )
                .maxOption
                .getOrElse(0)
            }
            .maxOption
            .getOrElse(0)

        }
      )
    }

    findBest(Set.empty, Set(), rooms("AA"), 30)
  }
  override def ans2(input: String) = ???
}
