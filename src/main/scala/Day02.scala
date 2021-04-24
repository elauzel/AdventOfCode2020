object Day02 {
  case class PasswordAndPolicy(minOccurrances: Int,
                          maxOccurrances: Int,
                          characterOccurring: Char,
                          password: String)

  def passwordMeetsCompanyPolicy(passwordAndPolicy: PasswordAndPolicy): Boolean = {
    val count = passwordAndPolicy.password.count(_ == passwordAndPolicy.characterOccurring)
    (count >= passwordAndPolicy.minOccurrances) && (count <= passwordAndPolicy.maxOccurrances)
  }

  def parsePasswordAndPolicy(line: String): PasswordAndPolicy = {
    val lineParts = line.split(" ")
    val frequencyPart = lineParts.head.split("-")
    val min = frequencyPart.head.toInt
    val max = frequencyPart.last.toInt
    val character = lineParts(1).head
    val password = lineParts.last
    PasswordAndPolicy(min, max, character, password)
  }

  def countPasswordsMeetingCompanyPolicy(lines: Vector[String]): Long =
    lines.count { line =>
      val passwordAndPolicy = parsePasswordAndPolicy(line)
      passwordMeetsCompanyPolicy(passwordAndPolicy)
    }

  def main(args: Array[String]): Unit =
    FileUtil.readResource("Day02.txt") match {
      case Left(exception) => throw exception
      case Right(lines) =>
        // Part 1 - How many passwords are valid according to historical company policies?
        val part1Answer = countPasswordsMeetingCompanyPolicy(lines)
        System.out.println(part1Answer)
        // Part 2 - ???
//        val part2Answer = ???
//        System.out.println(part2Answer)
    }
}
