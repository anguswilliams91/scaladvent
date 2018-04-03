import scala.io.Source
import scala.collection.mutable

object puzzleSix {

  def main(args: Array[String]): Unit = {

    type Allocations = List[Int]

    def updateAllocations(allocs: Allocations): Allocations = {

      def redistributeBlocks(allocs: Allocations, blocks: Int, index: Int): Allocations = {
        if (blocks == 0) allocs
        else {
          redistributeBlocks(allocs updated (index, allocs(index) + 1), blocks - 1, (index + 1) % allocs.size)
        }
      }

      val maxInd = allocs.indices.maxBy(allocs)
      redistributeBlocks(allocs updated (maxInd, 0), allocs.max, (maxInd + 1) % allocs.size)
    }

  def countCycles(allocs: Allocations): (Int, Int) = {

    def countCyclesAcc(allocs: Allocations, acc: Int, since: Map[Allocations, Int]): (Int, Int) = {
      if (since contains allocs) (acc, since(allocs))
      else {
        countCyclesAcc(updateAllocations(allocs), acc + 1, since.mapValues(_ + 1) updated (allocs, 1))
      }
    }
    countCyclesAcc(allocs, 0, Map[Allocations, Int]())
  }

  println(countCycles(List(0, 5, 10, 0,	11,	14,	13,	4, 11, 8,	8, 7,	1, 4,	12,	11)))

  }

}