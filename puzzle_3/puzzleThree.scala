import scala.io.Source

object puzzleThree {

  def main(args: Array[String]): Unit = {

    val n = 10
    lazy val turns = (for {
      i <- (1 to n)
      j <- "r"*i + "u"*i + "l"*(i + 1) + "d"*(i + 1)
      if i % 2 != 0
    } yield j).toList.reverse.toStream

    def changePosition(pos: (Int, Int), move: Char): (Int, Int) = move match {
      case 'r' => (pos._1 + 1, pos._2)
      case 'l' => (pos._1 - 1, pos._2)
      case 'u' => (pos._1, pos._2 + 1)
      case 'd' => (pos._1, pos._2 - 1)
    }

    def findPosition(moves: Stream[Char]): (Int, Int) = {

      def findPositionAcc(moves: Stream[Char], currentPos: (Int, Int)): (Int, Int) = moves match {
        case Stream.Empty => currentPos
        case x #:: xs => findPositionAcc(xs, changePosition(currentPos, x))
      }
      findPositionAcc(moves, (0, 0))
    }

    println(findPosition(turns))

    }

  }