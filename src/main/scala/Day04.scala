import scala.collection.mutable

object Day04 {
  private val RequiredPassportFields = Vector("ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt") // cid is optional

  def isValidPassport(line: String): Boolean = {
    val passportData = line.split(" ")
      .map(entry => {
        val keyAndValue = entry.split(":")
        keyAndValue.head -> keyAndValue.last
      }).toMap
    RequiredPassportFields.forall(passportData.contains)
  }

  def parsePassportLines(lines: Vector[String]): Vector[String] = {
    val passports = mutable.ListBuffer.empty[String]
    var currentPassport = mutable.ListBuffer.empty[String]
    lines.foreach { line =>
      if (line.isEmpty) {
        passports += currentPassport.mkString(" ")
        currentPassport = mutable.ListBuffer.empty[String]
      } else currentPassport += line
    }
    (passports += currentPassport.mkString(" ")).filter(_.nonEmpty).toVector
  }

  def main(args: Array[String]): Unit =
    FileUtil.readResource("Day04.txt") match {
      case Left(exception) => throw exception
      case Right(lines) =>
        // Part 1 - How many passports are valid after modifying the scanner to ignore cid?
        val part1Answer = parsePassportLines(lines)
          .map(isValidPassport)
          .count(b => b)
        System.out.println(part1Answer)
      // Part 2 - ???
      //        val part2Answer = ???
      //        System.out.println(part2Answer)
    }
}
