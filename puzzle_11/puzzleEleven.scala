import scala.io.Source

object puzzleEleven {

  type HexPoint = (Int, Int, Int)

  def move(location: HexPoint, instruction: String): HexPoint = {
    val (x, y, z) = location
    instruction match {
      case "n" => (x, y + 1, z - 1)
      case "s" => (x, y - 1, z + 1)
      case "ne" => (x + 1, y, z - 1)
      case "nw" => (x - 1, y + 1, z)
      case "se" => (x + 1, y - 1, z)
      case "sw" => (x - 1, y, z + 1)
    }
  }

  def distance(location: HexPoint, other: HexPoint): Int =
    (math.abs(location._1 - other._1) + math.abs(location._2 - other._2) + math.abs(location._3 - other._3))/2

  def main(args: Array[String]): Unit = {

    val instructions = Source.fromFile("puzzle_11_input.txt").mkString.trim.split(',')
    val history = instructions.foldLeft(List(((0, 0, 0), 0))) {
      (history, inst) => {
        val newPoint = move(history.head._1, inst)
        (newPoint, distance(newPoint, (0, 0, 0))) :: history
      }
    }
    val furthest = history.reduceLeft((location, other) => if (location._2 > other._2) location else other)
    println("Distance at the end = " + history.head._2)
    println("Furthest from start point = " + furthest._2)
  }

}
