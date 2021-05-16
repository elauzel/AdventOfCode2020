import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day10Spec extends AnyFlatSpec with Matchers {
  "findJoltageDifferences" should "return no differences when the bag has no adapters" in {
    Day10.findJoltageDifferences(Vector.empty[Int]) should be(Map.empty[Int, Int])
  }

  "findJoltageDifferences" should "use all adapters in your bag to connect your device to the wall, counting the joltage differences between each connection" in {
    val adapters1 = Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    Day10.findJoltageDifferences(adapters1) should be(Map(1 -> 10, 3 -> 1))
    val adapters2 = Vector(1, 2, 3, 5, 8, 13, 21, 34, 55)
    val differences2 = Map(1 -> 3, 2 -> 1, 3 -> 2, 5 -> 1, 8 -> 1, 13 -> 1, 21 -> 1)
    Day10.findJoltageDifferences(adapters2) should be(differences2)
    val adapters3 = Vector(16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4)
    val differences3 = Map(1 -> 7, 3 -> 5)
    Day10.findJoltageDifferences(adapters3) should be(differences3)
  }

  "findJoltageProduct" should "multiply the count of found joltage differences for the supplied joltages" in {
    val adapters1 = Vector(1, 2, 3, 5, 8, 13, 21, 34, 55)
    Day10.findJoltageProduct(adapters1, Vector(1, 2)) should be(3)
    Day10.findJoltageProduct(adapters1, Vector(1, 3)) should be(6)
    Day10.findJoltageProduct(adapters1, Vector(2, 3)) should be(2)

    val adapters2 = Vector(16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4)
    Day10.findJoltageProduct(adapters2, Vector(1, 3)) should be(35)
  }

  "findJoltageProduct" should "return zero if a supplied joltage is not found in the differences" in {
    val adapters1 = Vector(1, 2, 3, 5, 8, 13, 21, 34, 55)
    Day10.findJoltageProduct(adapters1, Vector(2, 999)) should be(0)
  }

  // Part 2

  "findPossibleConnections" should "return a map of adapters any given adapter could connect to" in {
    val adapters1 = Vector(16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4)
    // adapters:  16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4, 0
    // sorted:    WALL 1 4 5 6 7 10 11 12 15 16 19 DEVICE
    val expected1 =
    Map(
      0 -> Vector(1), // wall
      1 -> Vector(4),
      4 -> Vector(5, 6, 7),
      5 -> Vector(6, 7),
      6 -> Vector(7),
      7 -> Vector(10),
      10 -> Vector(11, 12),
      11 -> Vector(12),
      12 -> Vector(15),
      15 -> Vector(16),
      16 -> Vector(19),
      19 -> Vector(22) // device
    )
    Day10.findPossibleConnections(adapters1) should be(expected1)

    val adapters2 = Vector(28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3)
    // adapters:  28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3
    // sorted:    WALL 1 2 3 4 7 8 9 10 11 14 17 18 19 20 23 24 25 28 31 32 33 34 35 38 39 42 45 46 47 48 49 DEVICE
    val expected2 =
    Map(
      0 -> Vector(1, 2, 3), // wall
      1 -> Vector(2, 3, 4),
      2 -> Vector(3, 4),
      3 -> Vector(4),
      4 -> Vector(7),
      7 -> Vector(8, 9, 10),
      8 -> Vector(9, 10, 11),
      9 -> Vector(10, 11),
      10 -> Vector(11),
      11 -> Vector(14),
      14 -> Vector(17),
      17 -> Vector(18, 19, 20),
      18 -> Vector(19, 20),
      19 -> Vector(20),
      20 -> Vector(23),
      23 -> Vector(24, 25),
      24 -> Vector(25),
      25 -> Vector(28),
      28 -> Vector(31),
      31 -> Vector(32, 33, 34),
      32 -> Vector(33, 34, 35),
      33 -> Vector(34, 35),
      34 -> Vector(35),
      35 -> Vector(38),
      38 -> Vector(39),
      39 -> Vector(42),
      42 -> Vector(45),
      45 -> Vector(46, 47, 48),
      46 -> Vector(47, 48, 49),
      47 -> Vector(48, 49),
      48 -> Vector(49),
      49 -> Vector(52), // device
    )
    Day10.findPossibleConnections(adapters2) should be(expected2)
  }

  "countPossibleAdapterArrangements" should "count the unique number of adapter arrangements to connect your device to the wall" in {
    // adapters:  16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4, 0
    // sorted:    WALL 1 4 5 6 7 10 11 12 15 16 19 DEVICE

    // possibilities = 8

    // WALL (1 4) 5 6 (7 10) 11 (12 15 16 19) DEVICE
    // WALL (1 4) 5 6 (7 10)    (12 15 16 19) DEVICE
    // WALL (1 4) 5   (7 10) 11 (12 15 16 19) DEVICE
    // WALL (1 4) 5   (7 10)    (12 15 16 19) DEVICE
    // WALL (1 4)   6 (7 10) 11 (12 15 16 19) DEVICE
    // WALL (1 4)   6 (7 10)    (12 15 16 19) DEVICE
    // WALL (1 4)     (7 10) 11 (12 15 16 19) DEVICE
    // WALL (1 4)     (7 10)    (12 15 16 19) DEVICE

    val adapters1 = Vector(16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4)
    Day10.countPossibleAdapterArrangements(adapters1) should be(8)

    // adapters:  28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3
    // sorted:    WALL 1 2 3 4 7 8 9 10 11 14 17 18 19 20 23 24 25 28 31 32 33 34 35 38 39 42 45 46 47 48 49 DEVICE

    // possibilities = 19208

    val adapters2 = Vector(28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3)
    Day10.countPossibleAdapterArrangements(adapters2) should be(19208)
  }
}
