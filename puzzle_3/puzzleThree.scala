import scala.collection.mutable

object puzzleThree {

  def main(args: Array[String]): Unit = {

    type Location = (Int, Int)

    def makeNats(n: Int): Stream[Int] = {
      n #:: makeNats(n + 1)
    }

    def nats: Stream[Int] = makeNats(1)
    def odds: Stream[Int] = nats.filter(_ % 2 != 0)

    def turns: Stream[Char] = for {
      i <- odds
      j <- "r"*i + "u"*i + "l"*(i + 1) + "d"*(i + 1)
    } yield j

    def changeState(move: Char, location: Location): Location = move match {
      case 'r' => (location._1 + 1, location._2)
      case 'l' => (location._1 - 1, location._2)
      case 'u' => (location._1, location._2 + 1)
      case 'd' => (location._1, location._2 - 1)
    }

    def findLocation(n: Int): Location = {
      def findLocationAcc(moves: => Stream[Char], location: Location): Location = moves match {
        case Stream.Empty => location
        case move #:: rest => findLocationAcc(rest, changeState(move, location))
      }
      findLocationAcc(turns.take(n - 1), (0, 0))
    }

    // First part: find Manhatten distance of given number from origin

    def findDistance(n: Int): Int = {
      val location = findLocation(n)
      math.abs(location._1) + math.abs(location._2)
    }

    println(findDistance(265149))

    // Second part: find first value that exceeds given input

    def isNeighbour(location: Location, other: Location): Boolean = {
      val (x0, y0) = location
      val (x1, y1) = other
      (math.abs(x1 - x0) <= 1) && (math.abs(y1 - y0) <= 1)
    }

    def locations: Stream[Location] = for (i <- nats) yield findLocation(i)
    val valueCache = mutable.Map[Location, Int]()

    def getValue(location: Location, prevLocs: Stream[Location], prevVals: Stream[Int]): Int = {
      valueCache getOrElseUpdate (
        location,
        (for {
          (value , prevLoc) <- prevVals zip prevLocs if isNeighbour(location, prevLoc)
        } yield value).foldLeft(0)(_ + _)
      )
    }

    def valueFromIndex(n: Int): Int = n match {
      case 1 => 1
      case _ => getValue(findLocation(n),
        locations.take(n - 1),
        for (i <- nats.take(n - 1)) yield valueFromIndex(i)
      )
    }

    val values: Stream[Int] = for (i <- nats) yield valueFromIndex(i)
    values.filter(_ > 265149).take(1) foreach println

  }

}
