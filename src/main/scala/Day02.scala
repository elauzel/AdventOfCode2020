import scala.util.Try

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

  def parsePasswordAndOccurrancePolicy(line: String): Option[PasswordAndOccurrancePolicy] =
    parsePasswordAndPolicyInfo(line)
      .map(info => PasswordAndOccurrancePolicy(info._1, info._2, info._3, info._4))

  def parsePasswordAndPositionPolicy(line: String): Option[PasswordAndPositionPolicy] =
    parsePasswordAndPolicyInfo(line)
      .map(info => PasswordAndPositionPolicy(info._1, info._2, info._3, info._4))

  private def parsePasswordAndPolicyInfo(line: String) =
    Try {
      line match {
        case s"$num1-$num2 $character: $password" => Some((num1.toInt, num2.toInt, character.head, password))
        case _ => None
      }
    }.getOrElse(None)

  def countPasswordsMeetingOccurrancePolicy(lines: Vector[String]): Long =
    lines.count { line =>
      parsePasswordAndOccurrancePolicy(line)
        .map(passwordMeetsCompanyOccurrancePolicy) match {
        case Some(result) => result
        case _ => false
      }
    }

  def countPasswordsMeetingPositionPolicy(lines: Vector[String]): Long =
    lines.count { line =>
      parsePasswordAndPositionPolicy(line)
        .map(passwordMeetsCompanyPositionPolicy) match {
        case Some(result) => result
        case _ => false
      }
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
