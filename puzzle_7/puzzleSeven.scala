import scala.io.Source

object puzzleSeven {

  def main(args: Array[String]): Unit = {

    val nameRegex = """([a-z]+)""".r
    val weightRegex = """(\d+)""".r

    def makeSubtree(input: String): (String, Int, List[String]) = {
      val names = nameRegex.findAllIn(input)
      val weight = weightRegex.findAllIn(input).map(_.toInt)
      val thisName = names.next
      val thisWeight = weight.next
      val childTrees = names.toList
      if (childTrees.nonEmpty) (thisName, thisWeight, childTrees)
      else (thisName, thisWeight, Nil)
    }

    def parseData(path: String): List[(String, Int, List[String])] = {
      Source.fromFile(path).getLines.toList.map(makeSubtree)
    }

    def findRoot(parsedData: List[(String, Int, List[String])]): String = {
      val parents = parsedData.map(_._1).toSet
      val children = parsedData.flatMap(_._3).toSet
      (parents diff children).head
    }

    println(findRoot(parseData("puzzle_7_input.txt")))



  }

}