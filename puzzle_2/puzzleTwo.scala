import scala.io.Source

object puzzleTwo {

  def main(args: Array[String]): Unit = {

    def getCheckSum(x: List[List[Int]]): Int = (for {row <- x} yield (row.max - row.min)).toList.foldLeft(0)(_ + _)

    def getEvenDivisors(x: List[List[Int]]): Int = {
      (for {
        row <- x
        i <- row
        j <- row
        if((i % j == 0) && (i > j))
      } yield i / j).toList.foldLeft(0)(_ + _)
    }

    val data = (for {
      line <- Source.fromFile("puzzle_2_input.txt").getLines
    } yield line.split("\t").map(_.toInt).toList).toList

    println(getCheckSum(data))
    println(getEvenDivisors(data))

  }
}