import jdk.internal.jimage.decompressor.SignatureParser.ParseResult
import lib.{Grid, Point}

import scala.util.parsing.combinator.RegexParsers

object Day15 extends Shared {
  type SensorBeacon = (Point, Point)

  private def parseInput(input: String): Array[SensorBeacon] = {
    input
      .split("\n")
      .map { case s"Sensor at x=$sx, y=$sy: closest beacon is at x=$bx, y=$by" =>
        val sensor = Point(sx.toInt, sy.toInt)
        val beacon = Point(bx.toInt, by.toInt)
        (sensor, beacon)
      }
  }

  def naive(input: String, y: Int) = {
    val (grid, sensors, beacons) = parseInput(input)
      .foldLeft((Grid.empty[Char], Grid.empty[Char], Grid.empty[Char])) {
        case ((grid, sensors, beacons), (sensor, beacon)) =>
          val dist = (beacon - sensor).taxicabDistance
          val noBeacons = for {
            yd <- (-dist) to dist
            xd = dist - yd.abs
            p <- (sensor + Point(-xd, yd)) to (sensor + Point(xd, yd))
          } yield p
          (
            grid.fill(noBeacons, '#'),
            sensors.updated(sensor, 'S'),
            beacons.updated(beacon, 'B')
          )
      }
    val ticks =
      Grid.empty[Char].updated(Point(-10, 0), '0').updated(Point(-10, 5), '5')
    println((grid ++ sensors ++ beacons ++ ticks).draw(identity))
    (grid ++ beacons).points.count { case (p, v) =>
      p.y == y && v == '#'
    }
  }

  private def extent(pairs: Array[SensorBeacon]) = {
    pairs
      .map { case (sensor, beacon) =>
        val dist = (sensor - beacon).taxicabDistance
        (sensor.x - dist) to (sensor.x + dist)
      }
      .reduce((a, b) => a.start.min(b.start) to a.last.max(b.max))
  }

  def ansY(input: String, ySearch: Int) = {
    val pairs = parseInput(input)
    extent(pairs)
      .count { x =>
        pairs.exists { case (sensor, beacon) =>
          val p = Point(x, ySearch)
          val dist = (sensor - beacon).taxicabDistance
          val sensorToP = (sensor - p).abs
          val xRange = dist - sensorToP.y
          xRange > 0 && sensorToP.x <= xRange && p != beacon
        }
      }
  }

  case class Line(p1: Point, p2: Point) {
    def d: Point = {
      val d = p2 - p1
      Point(d.x / d.x.abs, d.y / d.y.abs)
    }
    assert(d.x.abs == d.y.abs, "Only diagonals supported")

    def intersect(other: Line): Option[Point] = {
      // p1 + d * k1 = other + other.d * k2
//      p1.x + d.x * k1 = other.x + other.d.x * k2
//      p1.y + d.y * k1 = other.y + other.d.y * k2
//      k1 = (other.x + other.d.x * k2 - p1.x) / d.x
//      k1 = (other.y + other.d.y * k2 - p1.y) / d.y
//      (other.x + other.d.x * k2 - p1.x) / d.x = (other.y + other.d.y * k2 - p1.y) / d.y
//      other.d.x * k2 / d.x - other.d.y * k2 /d.y = other.y/d.y - p1.y / d.y - other.x/d.x + p1.x / d.
//      k2 = (other.y/d.y - p1.y / d.y - other.x/d.x + p1.x / d.x) / (other.d.x / d.x - other.d.y /d.y)
      val k2 =
        (other.p1.y / d.y - p1.y / d.y - other.p1.x / d.x + p1.x / d.x) / (other.d.x / d.x - other.d.y / d.y)
      println(k2)
      Some(other.p1 + Point(other.d.x * k2, other.d.y * k2))
    }
  }

  def intersections(p1: SensorBeacon, p2: SensorBeacon): List[Point] = {
    val (sensor1, beacon1) = p1
    val (sensor2, beacon2) = p2
    val dist1 = (sensor1 - beacon1).taxicabDistance
    val dist2 = (sensor2 - beacon2).taxicabDistance

    // sensor1 + Point(dist1 - k1, k1) = sensor2 + Point(dist2 - k2, -k2)
    // sensor1 - Point(dist1 - k1, -k1) = sensor2 - Point(dist2 - k2, k2)
    // or reversed
    // k1 >= 0, k2 >= 0

    // sensor1.x + dist1 - k1 = sensor2.x + dist2 - k2
    val k1 = (sensor2.y - sensor1.y + sensor1.x - sensor2.x + dist1 - dist2) / 2
    if (k1 < dist1) {
      List(sensor1 + Point(dist1 - k1, k1), sensor1 + Point(-dist1 + k1, k1))
    } else {
      List.empty
    }
  }

  def ans2Y(input: String, extent: Range) = {
    val pairs = parseInput(input)
    val found = pairs
      .combinations(3)
      .collect { case Array(p1, p2, p3) =>
        (intersections(p1, p2) ++ intersections(p2, p3)).combinations(2).find { case List(p1, p2) =>
          (p1 - p2).taxicabDistance < 2
        }
      }
      .toList

    val (grid, sensors, beacons) = parseInput(input)
      .foldLeft((Grid.empty[Char], Grid.empty[Char], Grid.empty[Char])) {
        case ((grid, sensors, beacons), (sensor, beacon)) =>
          val dist = (beacon - sensor).taxicabDistance
          val noBeacons = for {
            yd <- (-dist) to dist
            xd = dist - yd.abs
            p <- (sensor + Point(-xd, yd)) to (sensor + Point(xd, yd))
          } yield p
          (
            grid.fill(noBeacons, '#'),
            sensors.updated(sensor, 'S'),
            beacons.updated(beacon, 'B')
          )
      }
    val ticks =
      Grid.empty[Char].updated(Point(-10, 0), '0').updated(Point(-10, 5), '5')
    val foundGrid = Grid.fill(found.flatMap(_.toList).flatten, 'X')
    println((grid ++ sensors ++ beacons ++ ticks ++ foundGrid).draw(identity))

  }

  override def ans(input: String) = ansY(input, ySearch = 2000000)
  override def ans2(input: String) = ans2Y(input, extent = 0 to 4_000_000)
}
