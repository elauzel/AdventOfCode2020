import scala.annotation.tailrec

object Problem01 {
  def findProductOfTwoSummedNumbers(desiredSum: Int, numbers: Vector[Int]): Either[Throwable, Long] = {
    @tailrec
    def go(desiredSum: Int, augend: Int, addendsToCheck: Vector[Int]): Either[Throwable, Long] = {
      addendsToCheck
        .find(_ + augend == desiredSum) match {
        case Some(addend) =>
          Right(augend * addend)
        case None =>
          if (addendsToCheck.size < 2)
            Left(new RuntimeException("no remaining numbers to check"))
          else
            go(desiredSum, addendsToCheck.head, addendsToCheck.tail)
      }
    }

    if (numbers.size < 2) Left(new RuntimeException("need at least two numbers to sum"))
    else go(desiredSum, numbers.head, numbers.tail)
  }

  def main(args: Array[String]): Unit =
  // Part 1 - Find the two numbers that sum to 2020 then multiply them together
    FileUtil.readResource("Problem01.txt") match {
      case Left(exception) => throw exception
      case Right(lines) =>
        val numbers = lines.map(_.toInt)
        val answer = findProductOfTwoSummedNumbers(2020, numbers)
        System.out.println(answer)
    }
}
