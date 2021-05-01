import Day08.{Acc, Command, Jmp, Nop}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day08Spec extends AnyFlatSpec with Matchers {
  "parseCommands" should "parse jmp, acc, and nop commands followed by integers" in {
    val commands = Vector(Nop(3), Acc(-7), Jmp(4))
    Day08.parseCommands(Vector("nop +3", "acc -7", "jmp +4")) should be((commands, Vector.empty[String]))
  }

  "parseCommands" should "not parse anything else" in {
    val badLines = Vector("bad, +7", "acc a", "acc &", "acc +7.5")
    Day08.parseCommands(badLines) should be((Vector.empty[Command], badLines))
  }

  // Part 1

  "attemptBootAndAccumulate" should "return the command execution order and the accumulator value for a program that is an infinite loop" in {
    val commands = Vector(Nop(0), Acc(1), Jmp(4), Acc(3), Jmp(-3), Acc(-99), Acc(1), Jmp(-4), Acc(6))
    val visitationOrder = Vector(Nop(0), Acc(1), Jmp(4), Acc(1), Jmp(-4), Acc(3), Jmp(-3), Acc(1))
    Day08.attemptBootAndAccumulate(commands) should be((5, visitationOrder, true))
  }

  "attemptBootAndAccumulate" should "return the command execution order and the accumulator value for a program that is not an infinite loop" in {
    val commands = Vector(Nop(0), Acc(1), Jmp(4), Acc(3), Jmp(-3), Acc(-99), Acc(1), Nop(-4), Acc(6))
    val visitationOrder = Vector(Nop(0), Acc(1), Jmp(4), Acc(1), Nop(-4), Acc(6))
    Day08.attemptBootAndAccumulate(commands) should be((8, visitationOrder, false))
  }

  // Part 2

  "repairInfiniteLoopToBootNormally" should "change exactly one nop to jmp (or vice versa) to fix an infinite loop" in {
    val commands = Vector(Nop(0), Acc(1), Jmp(4), Acc(3), Jmp(-3), Acc(-99), Acc(1), Jmp(-4), Acc(6))
    val visitationOrder = Vector(Nop(0), Acc(1), Jmp(4), Acc(1), Nop(-4), Acc(6))
    Day08.repairInfiniteLoopToBootNormally(commands) should be((8, 7, visitationOrder))
  }
}
