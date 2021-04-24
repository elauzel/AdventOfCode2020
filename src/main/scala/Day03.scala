import Day03.countTreesOnSlope

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

  def countTreesOnSlopes(slopes: Seq[(Int, Int)], landscape: Vector[Vector[Boolean]]): Seq[Long] =
    slopes.map(s => countTreesOnSlope(s._1, s._2, landscape))

  def main(args: Array[String]): Unit =
    FileUtil.readResource("Day03.txt") match {
      case Left(exception) => throw exception
      case Right(lines) =>
        val landscape = lines.map(parseLineIntoTreeLocations)
        // Part 1 - Starting at the top-left corner of your map and following a slope of right 3 and down 1, how many trees would you encounter?
        val part1Answer = countTreesOnSlope(3, 1, landscape)
        System.out.println(part1Answer)
        val slopes = Vector((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
        // Part 2 - Determine the number of trees you would encounter if, for each of the following slopes,
        // you start at the top-left corner and traverse the map all the way to the bottom, then get their product:
        val part2Answer = countTreesOnSlopes(slopes, landscape).product
        System.out.println(part2Answer)
    }
}
