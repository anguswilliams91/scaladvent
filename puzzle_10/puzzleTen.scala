import scala.io.Source
import scala.annotation.tailrec

object puzzleTen {

  def main(args: Array[String]): Unit = {

    type HashList = List[Int]

    def replaceSublist(list: HashList, indices: IndexedSeq[Int], replacement: IndexedSeq[Int]): HashList =
      indices.foldLeft(list)((toUpdate, index) => toUpdate updated(index, replacement(indices indexOf index)))

    def update(list: HashList, current: Int, length: Int): HashList = {
      val reverseInds = (current until current + length).map(_ % list.size)
      val replacement = reverseInds.map(list(_)).reverse
      replaceSublist(list, reverseInds, replacement)
    }

    def makeHash(instructions: List[Int]) = {

      @tailrec
      def makeHashRec(hashList: HashList, instructions: List[Int], current: Int, skip: Int): HashList = {
        instructions match {
          case Nil => hashList
          case length :: rest => makeHashRec(update(hashList, current, length), rest, current + length + skip, skip + 1)
        }
      }

      makeHashRec((0 until 256).toList, instructions, 0, 0)

    }

    // Part 1
    val instructions = List(70, 66, 255, 2, 48, 0, 54, 48, 80, 141, 244, 254, 160, 108, 1, 41)
    val hashList = makeHash(instructions)
    println(hashList(0) * hashList(1))

    // Part 2
    val instructionString = "70,66,255,2,48,0,54,48,80,141,244,254,160,108,1,41"
    val instructionSegment = instructionString.map(_.toByte.toInt).toList ++ List(17, 31, 73, 47, 23)
    val instructions64 = (1 to 63).foldLeft(instructionSegment)((lengths, _) => lengths ++ instructionSegment)
    val hash = makeHash(instructions64).grouped(16).map(_.reduce(_ ^ _)).map(_.toHexString).reduce(_ + _)
    println(hash)

  }

}