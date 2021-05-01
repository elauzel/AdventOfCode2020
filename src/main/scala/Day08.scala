import scala.annotation.tailrec
import scala.collection.mutable

object Day08 {
  sealed class Command(number: Int)
  case class Nop(number: Int) extends Command(number)
  case class Jmp(number: Int) extends Command(number)
  case class Acc(number: Int) extends Command(number)

  def attemptBootAndAccumulate(commands: Seq[Command]): (Int, Vector[Command], Boolean) = {
    val alreadyExecuted = mutable.ListBuffer.from(commands.map(_ => false))

    @tailrec
    def go(executionOrder: Vector[Command] = Vector.empty[Command],
           instructionIndex: Int = 0,
           acc: Int = 0): (Int, Vector[Command], Boolean) =
      if (instructionIndex < commands.length) {
        val instruction = commands(instructionIndex)
        if (alreadyExecuted(instructionIndex)) {
          (acc, executionOrder :+ instruction, true)
        } else {
          alreadyExecuted(instructionIndex) = true
          instruction match {
            case Nop(_) => go(executionOrder :+ instruction, instructionIndex + 1, acc)
            case Acc(number) => go(executionOrder :+ instruction, instructionIndex + 1, acc + number)
            case Jmp(number) => go(executionOrder :+ instruction, instructionIndex + number, acc)
          }
        }
      } else {
        (acc, executionOrder, false)
      }

    go()
  }

  def repairInfiniteLoopToBootNormally(commands: Vector[Command]): (Int, Int, Vector[Command]) = {
    @tailrec
    def go(instructionIndex: Int = 0): (Int, Int, Vector[Command]) = {
      commands(instructionIndex) match {
        case Nop(number) =>
          val newCommands = mutable.ListBuffer.from(commands)
          newCommands(instructionIndex) = Jmp(number)
          val result = attemptBootAndAccumulate(newCommands.toSeq)
          if (result._3) go(instructionIndex + 1)
          else (result._1, instructionIndex, result._2)
        case Jmp(number) =>
          val newCommands = mutable.ListBuffer.from(commands)
          newCommands(instructionIndex) = Nop(number)
          val result = attemptBootAndAccumulate(newCommands.toSeq)
          if (result._3) go(instructionIndex + 1)
          else (result._1, instructionIndex, result._2)
        case _ => go(instructionIndex + 1)
      }
    }

    go()
  }

  def parseCommands(lines: Vector[String]): (Vector[Command], Vector[String]) = {
    val commands = mutable.ListBuffer.empty[Command]
    val badLines = mutable.ListBuffer.empty[String]
    lines.foreach { line =>
      val split = line.split(' ')
      try {
        split.head match {
          case "nop" => commands.append(Nop(split.last.toInt))
          case "jmp" => commands.append(Jmp(split.last.toInt))
          case "acc" => commands.append(Acc(split.last.toInt))
          case _ => badLines.append(line)
        }
      } catch {
        case _: Throwable => badLines.append(line)
      }
    }
    (commands.toVector, badLines.toVector)
  }

  def main(args: Array[String]): Unit =
    FileUtil.readResource("Day08.txt") match {
      case Left(exception) => throw exception
      case Right(lines) =>
        val commands = parseCommands(lines)._1
        // Part 1 - Immediately before any instruction is executed a second time, what value is in the accumulator?
        val part1Answer = attemptBootAndAccumulate(commands)
        System.out.println(part1Answer)
        // Part 2 - Fix the program so that it terminates normally by changing exactly one jmp (to nop) or nop (to jmp).
        // What is the value of the accumulator after the program terminates?
        val part2Answer = repairInfiniteLoopToBootNormally(commands)
        System.out.println(part2Answer)
    }
}
