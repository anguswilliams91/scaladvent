import scala.io.Source

object puzzleFive {

  def main(args: Array[String]): Unit = {

    type Instructions = List[Int]

    def escape(intructions: Instructions, secondPuzzle: Boolean = false): Int = {

        def update(instruction: Int): Int = {
          if (secondPuzzle && instruction >= 3) instruction - 1
          else instruction + 1
        }

      def moveAcc(current: Int, instructions: Instructions, acc: Int): Int = {
        if (current >= instructions.size || current < 0) acc
        else moveAcc(current + instructions(current),
          instructions updated (current, update(instructions(current))),
          acc + 1)
        }

      moveAcc(0, intructions, 0)

    }

    val data = Source.fromFile("puzzle_5_input.txt").getLines.toList.map(_.toInt)
    println(escape(data, secondPuzzle = false))
    println(escape(data, secondPuzzle = true))

  }

}