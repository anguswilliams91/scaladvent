import scala.io.Source
import scala.util.matching.Regex

object puzzleEight {

  case class Instruction(val register: String,
                         val increase: Boolean,
                         val change: Int,
                         val conditionRegister: String,
                         val op: String,
                         val value: Int) {

    lazy val inc = if (increase) "inc" else "dec"

    override def toString: String = register + " " + inc + " if " + conditionRegister + " " + op + " " + value

    val condFunc: (Int, Int) => Boolean = op match {
      case "<" => (x, y) => x < y
      case ">" => (x, y) => x > y
      case "<=" => (x, y) => x <= y
      case ">=" => (x, y) => x >= y
      case "==" => (x, y) => x == y
      case "!=" => (x, y) => x != y
    }

    val modFunc: (Int, Int) => Int = increase match {
      case true => (x, y) => x + y
      case false => (x, y) => x - y
    }

    def compute(regs: RegisterList): RegisterList = {
      if (condFunc(regs._1(conditionRegister), value)) {
        val newRegs = regs._1 updated (register, modFunc(regs._1(register), change))
        val newMax = newRegs.maxBy(_._2)._2
        val updatedMax = if (regs._2 < newMax) newMax else regs._2
        (newRegs, updatedMax)
      }
      else regs
    }
  }

  type RegisterList = (Map[String, Int], Int)

  def main(args: Array[String]): Unit = {

    val instructionRegex = """([a-z]+) (inc|dec) (\-*\d+) if ([a-z]+) (<|>|<=|>=|==|!=) (\-*\d+)""".r

    def parseRow(instruction: String): Instruction = {
      instruction match {
        case instructionRegex(register, increase, change, conditionRegister, op, value) =>
          Instruction(register, increase == "inc", change.toString.toInt, conditionRegister, op, value.toString.toInt)
      }
    }

    val data: List[Instruction] = Source.fromFile("puzzle_8_input.txt").getLines.map(parseRow).toList

    def getEmptyRegisters(input: List[Instruction]): RegisterList =
      ((data.map(x => x.register).toSet ++ data.map(x => x.conditionRegister).toSet).map(_ -> 0).toMap, 0)

    def applyInstructions(register: RegisterList, instructions: List[Instruction]): RegisterList = {
      instructions.foldLeft(register)((reg, inst) => inst.compute(reg))
    }

    //first part: find maximum value after all computation

    val finalRegisters = applyInstructions(getEmptyRegisters(data), data)
    println(finalRegisters._1.maxBy(_._2))

    //second part: find maximum value at any point
    println(finalRegisters._2)



  }

}