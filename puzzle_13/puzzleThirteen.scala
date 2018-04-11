import scala.io.Source

object puzzleThirteen {

  def caught(t: Int, range: Int): Boolean = t % (2 * range - 2) == 0

  def everCaught(range: Map[Int, Int], delay: Int): Boolean = {
    (range map {case (depth, range) => caught(delay + depth, range)} filter identity).size > 0
  }

  def calculateSeverity(range: Map[Int, Int], delay: Int): Int = {
    range.keys.filter(
      depth => caught(depth + delay, range(depth))
    ).map(
      depth => depth * range(depth)
    ).foldLeft(0)(_ + _)
  }

  def findMinimumWait(range: Map[Int, Int]): Int = {

    def makeNats(n: Int): Stream[Int] = {
      n #:: makeNats(n + 1)
    }

    (makeNats(0) filter {case delay => !everCaught(range, delay)}).take(1)(0)

  }

  def main(args: Array[String]): Unit = {

    val range = Source.fromFile("puzzle_13_input.txt").getLines.map(
      _.split(':').map(_.trim.toInt)
    ).map(x => x(0) -> x(1)).toMap
    println(calculateSeverity(range, 0))
    println(findMinimumWait(range))

  }

}