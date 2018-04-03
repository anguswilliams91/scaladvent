import scala.io.Source

object puzzleOne {

  def main(args: Array[String]): Unit = {

    def solveCaptcha(x: List[Int], skip: Int = 1): Int = {
      (for {
        j <- x.indices.filter(i => x(i) == x((i + skip) % x.size)).distinct
      } yield x(j)).toList.foldLeft(0)(_ + _)
    }

    val data = Source.fromFile("puzzle_1_input.txt").getLines.mkString.toList.map(x => x.toString.toInt)
    println(solveCaptcha(data, skip=1))
    println(solveCaptcha(data, skip=data.size/2))

  }

}