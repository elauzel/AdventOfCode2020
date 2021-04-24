import scala.annotation.tailrec

object Problem01 {
  def findProductOfTwoSummedNumbers(desiredSum: Long, numbers: Vector[Long]): Either[Throwable, Long] =
    if (numbers.size < 2)
      Left(new RuntimeException(s"no two numbers found with desired sum $desiredSum"))
    else {
      @tailrec
      def go(desiredSum: Long, augend: Long, addendsToCheck: Vector[Long]): Either[Throwable, Long] =
        addendsToCheck.find(_ + augend == desiredSum) match {
          case Some(addend) =>
            Right(augend * addend)
          case None =>
            if (addendsToCheck.size < 2)
              Left(new RuntimeException(s"no two numbers found with desired sum $desiredSum"))
            else
              go(desiredSum, addendsToCheck.head, addendsToCheck.tail)
        }

      go(desiredSum, numbers.head, numbers.tail)
  }

  @tailrec
  def findProductOfThreeSummedNumbers(desiredSum: Long, numbers: Vector[Long]): Either[Throwable, Long] =
    if (numbers.length < 3)
      Left(new RuntimeException(s"no three numbers found with desired sum $desiredSum"))
    else {
    val number = numbers.head
    findProductOfTwoSummedNumbers(desiredSum - number, numbers.tail) match {
      case Left(_) =>
        findProductOfThreeSummedNumbers(desiredSum, numbers.tail)
      case Right(product) =>
        Right(product * number)
    }
  }

  def main(args: Array[String]): Unit =
    FileUtil.readResource("Problem01.txt") match {
      case Left(exception) => throw exception
      case Right(lines) =>
        val numbers = lines.map(_.toLong)
        // Part 1 - Find the two numbers that sum to 2020 then multiply them together
        val part1Answer = findProductOfTwoSummedNumbers(2020, numbers)
        System.out.println(part1Answer)
        // Part 2 - Find the three numbers that sum to 2020 then multiply them together
        val part2Answer = findProductOfThreeSummedNumbers(2020, numbers)
        System.out.println(part2Answer)
    }
}
