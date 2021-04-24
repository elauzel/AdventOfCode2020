import scala.annotation.tailrec

object Day03 {
  def parseLineIntoTreeLocations(line: String): Vector[Boolean] =
    line.map(_ == '#').toVector

  def isTree(x: Int, y: Int, landscape: Vector[Vector[Boolean]]): Boolean = {
    val row = landscape(y)
    row(x % row.length)
  }

  @tailrec
  def countTreesOnSlope(stepsRight: Int,
                        stepsDown: Int,
                        landscape: Vector[Vector[Boolean]],
                        currentPositionX: Int = 0,
                        currentPositionY: Int = 0,
                        currentTreeCount: Long = 0): Long =
    if (currentPositionY >= landscape.length)
      currentTreeCount
    else
      countTreesOnSlope(stepsRight,
        stepsDown,
        landscape,
        currentPositionX + stepsRight,
        currentPositionY + stepsDown,
        if (isTree(currentPositionX, currentPositionY, landscape)) currentTreeCount + 1 else currentTreeCount)

  def main(args: Array[String]): Unit =
    FileUtil.readResource("Day03.txt") match {
      case Left(exception) => throw exception
      case Right(lines) =>
        val landscape = lines.map(parseLineIntoTreeLocations)
        // Part 1 - Starting at the top-left corner of your map and following a slope of right 3 and down 1, how many trees would you encounter?
        val part1Answer = countTreesOnSlope(3, 1, landscape)
        System.out.println(part1Answer)
      // Part 2 - ???
      //        val part2Answer = ???
      //        System.out.println(part2Answer)
    }
}
