object Day02 {
  case class PasswordAndOccurrancePolicy(minOccurrances: Int,
                                         maxOccurrances: Int,
                                         character: Char,
                                         password: String)

  case class PasswordAndPositionPolicy(index1: Int,
                                       index2: Int,
                                       character: Char,
                                       password: String)

  def passwordMeetsCompanyOccurrancePolicy(passwordAndPolicy: PasswordAndOccurrancePolicy): Boolean = {
    val count = passwordAndPolicy.password.count(_ == passwordAndPolicy.character)
    (count >= passwordAndPolicy.minOccurrances) && (count <= passwordAndPolicy.maxOccurrances)
  }

  def passwordMeetsCompanyPositionPolicy(passwordAndPolicy: PasswordAndPositionPolicy): Boolean = {
    val charAtIndex1 = passwordAndPolicy.password(passwordAndPolicy.index1 - 1) == passwordAndPolicy.character
    val charAtIndex2 = passwordAndPolicy.password(passwordAndPolicy.index2 - 1) == passwordAndPolicy.character
    charAtIndex1 ^ charAtIndex2
  }

  def parsePasswordAndOccurrancePolicy(line: String): PasswordAndOccurrancePolicy = {
    val info = parsePasswordAndPolicyInfo(line)
    PasswordAndOccurrancePolicy(info._1, info._2, info._3, info._4)
  }

  def parsePasswordAndPositionPolicy(line: String): PasswordAndPositionPolicy = {
    val info = parsePasswordAndPolicyInfo(line)
    PasswordAndPositionPolicy(info._1, info._2, info._3, info._4)
  }

  private def parsePasswordAndPolicyInfo(line: String) = {
    val lineParts = line.split(" ")
    val frequencyPart = lineParts.head.split("-")
    val number1 = frequencyPart.head.toInt
    val number2 = frequencyPart.last.toInt
    val character = lineParts(1).head
    val password = lineParts.last
    (number1, number2, character, password)
  }

  def countPasswordsMeetingOccurrancePolicy(lines: Vector[String]): Long =
    lines.count { line =>
      val passwordAndPolicy = parsePasswordAndOccurrancePolicy(line)
      passwordMeetsCompanyOccurrancePolicy(passwordAndPolicy)
    }

  def countPasswordsMeetingPositionPolicy(lines: Vector[String]): Long =
    lines.count { line =>
      val passwordAndPolicy = parsePasswordAndPositionPolicy(line)
      passwordMeetsCompanyPositionPolicy(passwordAndPolicy)
    }

  def main(args: Array[String]): Unit =
    FileUtil.readResource("Day02.txt") match {
      case Left(exception) => throw exception
      case Right(lines) =>
        // Part 1 - How many passwords meet company policies, using the two numbers as min/max character occurrances?
        val part1Answer = countPasswordsMeetingOccurrancePolicy(lines)
        System.out.println(part1Answer)
        // Part 2 - How many passwords meet company policies, using the two numbers as the index where character must occur only once?
        val part2Answer = countPasswordsMeetingPositionPolicy(lines)
        System.out.println(part2Answer)
    }
}
