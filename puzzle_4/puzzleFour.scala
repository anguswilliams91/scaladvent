import scala.io.Source

object puzzleFour {

  def main(args: Array[String]): Unit = {

    def countValidPasswords(passwords: List[String]): Int = {
      passwords.filter(
        password => {
          val words = password.split(" ")
          words.size == words.distinct.size
        }
      ).size
    }

    def toCharSet(password: String): Set[(Char, Int)] = {
      val chars = password.toList
      val uniqueChars = password.toSet
      (for (char <- uniqueChars) yield (char, chars.filter(_ == char).size)).toSet
      }

    def countAnagramPasswords(passwords: List[String]): Int = {
      passwords.map(_.split(" ").map(x => toCharSet(x))).filter {
        charSets => charSets.size == charSets.distinct.size
      }.size
    }

    val data = Source.fromFile("puzzle_4_input.txt").getLines.toList
    println(countValidPasswords(data))
    println(countAnagramPasswords(data))

  }

}