object Day06 {
  def countQuestionsAnsweredPerGroup(group: Vector[String]): Int =
    group.mkString("").distinct.size

  def main(args: Array[String]): Unit =
    FileUtil.readResourceAsGroupedLines("Day06.txt") match {
      case Left(exception) => throw exception
      case Right(lines) =>
        // Part 1 - For each group, count the number of questions to which anyone answered "yes". What is the sum of those counts
        val part1Answer = lines.map(countQuestionsAnsweredPerGroup).sum
        System.out.println(part1Answer)
      // Part 2 - ???
      //        val part2Answer = ???
      //        System.out.println(part2Answer)
    }
}
