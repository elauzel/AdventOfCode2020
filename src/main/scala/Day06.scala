object Day06 {
  def countQuestionsAffirmedByAnyone(group: Vector[String]): Int =
    group.mkString("").distinct.length

  def countQuestionsAffirmedByEveryone(group: Vector[String]): Int =
    group.mkString("")
      .distinct
      .count(c => group.forall(_.contains(c)))

  def main(args: Array[String]): Unit =
    FileUtil.readResourceAsGroupedLines("Day06.txt") match {
      case Left(exception) => throw exception
      case Right(lines) =>
        // Part 1 - For each group, count the number of questions to which anyone answered "yes". What is the sum of those counts?
        val part1Answer = lines.map(countQuestionsAffirmedByAnyone).sum
        System.out.println(part1Answer)
        // Part 2 - For each group, count the number of questions to which *everyone* answered "yes". What is the sum of those counts?
        val part2Answer = lines.map(countQuestionsAffirmedByEveryone).sum
        System.out.println(part2Answer)
    }
}
