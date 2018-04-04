import scala.io.Source
import scala.util.matching


object puzzleNine{

  def main(args: Array[String]): Unit = {

    def removeBangs(input: String): String =

    def removeGarbage(input: String): String = {
      val bangPattern = """!.{1}""".r
      val opener = """<""".r
      val closer = """>""".r
      val inputNoBangs = bangPattern.replaceAllIn(input, "")

      def removeGarbageRec(input: String): String = opener.findFirstMatchIn(input) match {
        case Some(openMatch) => {
          val closeMatch = closer.findFirstMatchIn(input).get
          val toRemove = input slice (openMatch.start, closeMatch.start + 1)
          removeGarbageRec(input.replace(toRemove, ""))
        }
        case None => input
      }

      removeGarbageRec(inputNoBangs)
    }


  }

}