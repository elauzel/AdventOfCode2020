import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day03Spec extends AnyFlatSpec with Matchers {
  "parse line into tree locations" should " convert . to false and # to true for each character" in {
    Day03.parseLineIntoTreeLocations("") should be(Vector.empty[Boolean])
    Day03.parseLineIntoTreeLocations(".") should be(Vector(false))
    Day03.parseLineIntoTreeLocations("#") should be(Vector(true))
    val locations = Vector(false, false, false, false, true, false, true, true, false)
    Day03.parseLineIntoTreeLocations("....#.##.") should be(locations)
  }

  "is tree" should "determine if landscape coordinate x,y has a tree" in {
    val landscape =
      Vector(
        Vector(false, false, false, false, true),
        Vector(false, true, false, false, true),
        Vector(false, false, true, false, false)
      )

    Day03.isTree(0, 0, landscape) should be(false)
    Day03.isTree(1, 0, landscape) should be(false)
    Day03.isTree(2, 0, landscape) should be(false)
    Day03.isTree(3, 0, landscape) should be(false)
    Day03.isTree(4, 0, landscape) should be(true)

    Day03.isTree(0, 1, landscape) should be(false)
    Day03.isTree(1, 1, landscape) should be(true)
    Day03.isTree(2, 1, landscape) should be(false)
    Day03.isTree(3, 1, landscape) should be(false)
    Day03.isTree(4, 1, landscape) should be(true)

    Day03.isTree(0, 2, landscape) should be(false)
    Day03.isTree(1, 2, landscape) should be(false)
    Day03.isTree(2, 2, landscape) should be(true)
    Day03.isTree(3, 2, landscape) should be(false)
    Day03.isTree(4, 2, landscape) should be(false)
  }

  "is tree" should "determine if landscape coordinate x,y has a tree, by wrapping around coordinates" in {
    val landscape =
      Vector(
        Vector(false, false, false, false, true),
        Vector(false, true, false, false, true),
        Vector(false, false, true, false, false)
      )

    Day03.isTree(5, 1, landscape) should be(false)
    Day03.isTree(6, 1, landscape) should be(true)
    Day03.isTree(7, 1, landscape) should be(false)
    Day03.isTree(8, 1, landscape) should be(false)
    Day03.isTree(9, 1, landscape) should be(true)
    Day03.isTree(10, 1, landscape) should be(false)
    Day03.isTree(11, 1, landscape) should be(true)
    Day03.isTree(12, 1, landscape) should be(false)
    Day03.isTree(13, 1, landscape) should be(false)
    Day03.isTree(14, 1, landscape) should be(true)
  }

  // Part 1

  "count trees on slope" should "count the number of trees encountered on a given slope until landscape height is exceeded" in {
    val lines = Vector(
    "....#...............#.#..###.##",
    ".#..#....###..............##...",
    "....###......#....#.#...#.##..#",
    ".......#........#..###...##....",
    ".....#..#......#..#..##..#...#."
    )
    val landscape = lines.map(Day03.parseLineIntoTreeLocations)
    Day03.countTreesOnSlope(1, 1, landscape) should be(1)
    Day03.countTreesOnSlope(2, 1, landscape) should be(2)
    Day03.countTreesOnSlope(3, 1, landscape) should be(1)
    Day03.countTreesOnSlope(4, 1, landscape) should be(1)
    Day03.countTreesOnSlope(5, 1, landscape) should be(0)
    Day03.countTreesOnSlope(6, 1, landscape) should be(0)
    Day03.countTreesOnSlope(7, 1, landscape) should be(1)
    Day03.countTreesOnSlope(8, 1, landscape) should be(0)
    Day03.countTreesOnSlope(9, 1, landscape) should be(3)
    Day03.countTreesOnSlope(10, 1, landscape) should be(2)
  }

  // Part 2

  "count trees on slopes" should "count the number of trees encountered on each slope" in {
    val lines = Vector(
      "....#...............#.#..###.##",
      ".#..#....###..............##...",
      "....###......#....#.#...#.##..#",
      ".......#........#..###...##....",
      ".....#..#......#..#..##..#...#."
    )
    val landscape = lines.map(Day03.parseLineIntoTreeLocations)
    val slopes = Vector((1, 1), (2, 1), (3, 1), (4, 1), (5, 1), (6, 1), (7, 1), (8, 1), (9, 1), (10, 1))
    Day03.countTreesOnSlopes(slopes, landscape) should be(Seq(1, 2, 1, 1, 0, 0, 1, 0, 3, 2))
  }
}
