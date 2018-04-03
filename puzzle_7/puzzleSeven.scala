import scala.io.Source
import scala.annotation.tailrec

object puzzleSeven {

  case class DiscTree(name: String, discWeight: Int, children: List[DiscTree]) {

    //Class representing trees of programs holding discs

    override def toString: String =
      "DiscTree(Name: " + name + ", Weight: " + discWeight + ", " + "Children: {" + children.map(_.toString).foldRight("")(_ + ", " + _) + " })"

    lazy val childWeights: List[Int] = {

      def treeListWeights(trees: List[DiscTree]): List[Int] = trees match {
        case Nil => Nil
        case x :: xs => (x.discWeight + treeListWeights(x.children).foldLeft(0)(_ + _)) :: treeListWeights(xs)
      }

      treeListWeights(children)
    }

    lazy val isBalanced: Boolean = childWeights.distinct.size == 1

    lazy val adjustment: Int = this.isBalanced match {
      case true => 0
      case false => {
        val valsAndCounts = childWeights.distinct.map(x => (x, childWeights.filter(y => y == x).size))
        valsAndCounts.filter(_._2 == 1)(0)._1 - valsAndCounts.filter(_._2 != 1)(0)._1
      }
    }

    lazy val brokenTree: Int =
      childWeights.indexOf(
        childWeights.distinct.map(x => (x, childWeights.filter(y => y == x).size)).filter(_._2 == 1)(0)._1
      )

    lazy val correctValue: Int = {

      @tailrec
      def correctValueRec(tree: DiscTree): Int = {
        val imbalanced = tree.isBalanced
        imbalanced match {
          case true => tree.discWeight - adjustment
          case false => correctValueRec(tree.children(tree.brokenTree))
        }
      }

      if (this.isBalanced) throw new Exception("This tree is balanced!")
      else correctValueRec(this.children(brokenTree))

    }

  }

  def main(args: Array[String]): Unit = {

    //Parse the data from the given format

    val nameRegex = """([a-z]+)""".r
    val weightRegex = """(\d+)""".r

    def parseRow(input: String): (String, Int, List[String]) = {
      val names = nameRegex.findAllIn(input)
      val weight = weightRegex.findAllIn(input).map(_.toInt)
      val thisName = names.next
      val thisWeight = weight.next
      val childTrees = names.toList
      if (childTrees.nonEmpty) (thisName, thisWeight, childTrees)
      else (thisName, thisWeight, Nil)
    }

    def parseData(path: String): List[(String, Int, List[String])] = {
      Source.fromFile(path).getLines.toList.map(parseRow)
    }

    // Part 1: find the root of the tree

    def findRoot(parsedData: List[(String, Int, List[String])]): String = {
      val parents = parsedData.map(_._1).toSet
      val children = parsedData.flatMap(_._3).toSet
      (parents diff children).head
    }

    val data = parseData("puzzle_7_input.txt")
    val root = findRoot(data)
    println(root)

    // Part 2: find the correct weight for the broken disc

    def makeTree(parsedData: List[(String, Int, List[String])]): DiscTree = {

      val root = findRoot(parsedData)

      def makeTreeRec(treeName: String): DiscTree = {
        val (name, discWeight, children) = parsedData.filter(_._1 == treeName)(0)
        children match {
          case Nil => DiscTree(name, discWeight, Nil)
          case _ => DiscTree(name, discWeight, children.map(makeTreeRec))
        }
      }

      makeTreeRec(root)
    }

    val tree = makeTree(data)
    println(tree.correctValue)

  }


}
