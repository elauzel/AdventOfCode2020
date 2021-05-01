import scala.annotation.tailrec

object Day09 {
  def findFirstNumberWithoutSum(numbers: Vector[Long], preambleLength: Int): Option[Long] = {
    numbers.sliding(preambleLength + 1).find { window => // (num1, num2, ... numn, desiredSum)
      !window.slice(0, preambleLength)
        .distinct
        .combinations(2)
        .exists(_.sum == window.last)
    }.map(_.last)
  }

  @tailrec
  def findContiguousNumbers(numbers: Vector[Long], desiredSum: Long, windowSize: Int = 2): Option[Vector[Long]] = {
    val contiguousNumbers = numbers.sliding(windowSize)
      .find(_.sum == desiredSum)
    if (contiguousNumbers.isEmpty) findContiguousNumbers(numbers, desiredSum, windowSize + 1)
    else Some(contiguousNumbers.toSeq.head)
  }

  def findEncryptionWeakness(contiguousNumbers: Vector[Long]): Long =
    contiguousNumbers.min + contiguousNumbers.max

  def main(args: Array[String]): Unit =
    FileUtil.readResource("Day09.txt") match {
      case Left(exception) => throw exception
      case Right(lines) =>
        val numbers = lines.map(_.toLong)
        // Part 1 - Find the first number in the list (after the preamble) which is not the sum of two of the 25 numbers before it.
        val part1Answer = findFirstNumberWithoutSum(numbers, 25)
        System.out.println(part1Answer)
        // Part 2 - What is the encryption weakness in your XMAS-encrypted list of numbers?
        val part2Answer = part1Answer
          .flatMap(findContiguousNumbers(numbers, _))
          .map(findEncryptionWeakness)
        System.out.println(part2Answer)
    }
}
