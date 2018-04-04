import scala.io.Source


object puzzleNine{

  def main(args: Array[String]): Unit = {

    val bangPattern = """!.{1}""".r
    def removeBangs(input: String): String = bangPattern.replaceAllIn(input, "")

    def removeGarbage(input: String): String = {
      val inputNoBangs = removeBangs(input)
      val opener = """<""".r
      val closer = """>""".r

      def removeGarbageAcc(input: String, pattern: Regex) = opener.findFirstMatchIn(input) match {
        case Some(patMatch) => {

        }
        case None => input
      }

      removeGarbageAcc(inputNoBangs, opener)
    }


  }

}