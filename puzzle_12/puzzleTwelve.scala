import scala.io.Source

object puzzleTwelve {

  def parseLine(line: String): Seq[Set[Int]] = {
    val splitLine = line.split("<->")
    val parentNode = splitLine(0).trim.toInt
    val childNodes = splitLine(1).split(',').map(_.trim.toInt)
    childNodes.map(Set(parentNode, _))
  }

  def findFriends(id: Int, pairs: Set[Set[Int]]): List[Int] = {
    val seedSet = pairs.filter(_ contains id)
    val seeds = seedSet.flatMap(_ - id)
    seeds.toList.flatMap(findFriends(_, pairs -- seedSet)) ++ seeds.toList
  }

  def findGroup(id: Int, pairs: Set[Set[Int]]): Set[Int] = findFriends(id, pairs).toSet + id

  def findAllGroups(pairs: Set[Set[Int]]): Set[Set[Int]] = {
    val programs = pairs.flatMap(_.toList).toSet
    programs.map(findGroup(_, pairs)).toSet
  }

  def main(args: Array[String]): Unit = {

    val data = Source.fromFile("puzzle_12_input.txt").getLines.flatMap(parseLine).toSet
    println(findGroup(0, data).size)
    println(findAllGroups(data).size)

  }

}