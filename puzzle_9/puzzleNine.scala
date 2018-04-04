import scala.io.Source
import scala.util.matching


object puzzleNine {

  def main(args: Array[String]): Unit = {

    def removeGarbage(input: String): (String, Int) = {
      val bangPattern = """!.{1}""".r
      val opener = """<""".r
      val closer = """>""".r
      val inputNoBangs = bangPattern.replaceAllIn(input, "")

      def removeGarbageRec(input: String, acc: Int): (String, Int) = input.indexOf('<') match {
        case -1 => (input, acc)
        case _ => {
          val leftPos = input indexOf '<'
          val rightPos = input indexOf '>'
          val before = input slice(0, leftPos)
          val after = input slice(rightPos + 1, input.size)
          removeGarbageRec(before + after, acc + rightPos - leftPos - 1)
        }
      }

      removeGarbageRec(inputNoBangs, 0)
    }

    def getScore(input: String): (Int, Int) = {
      val (cleanedInput, numChars) = removeGarbage(input)

      def getScoreRec(input: List[Char], numOpen: Int, score: Int): Int = input match {
        case Nil => score
        case x :: xs => {
          x match {
            case '{' => getScoreRec(xs, numOpen + 1, score)
            case '}' => getScoreRec(xs, numOpen - 1, score + numOpen)
            case _ => getScoreRec(xs, numOpen, score)
          }
        }
      }

      (getScoreRec(cleanedInput.toList, 0, 0), numChars)
    }

    val data = Source.fromFile("puzzle_9_input.txt").mkString
    println(getScore(data))


  }

}