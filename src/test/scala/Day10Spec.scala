import Day10.AdapterGroup
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

  "groupAdapters" should "group together all adapters that must join together" in {
    val adapters1 = Vector(16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4)
    // (1 4) 5 6 (7 10) 11 (12 15) (16 19)
    val adapterGroups1 = Vector(
      AdapterGroup(1, 4),
      AdapterGroup(5, 5),
      AdapterGroup(6, 6),
      AdapterGroup(7, 10),
      AdapterGroup(11, 11),
      AdapterGroup(12, 15),
      AdapterGroup(16, 19)
    )
    Day10.groupAdapters(adapters1) should be(adapterGroups1)
    val adapters2 = Vector(28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3)
    // 1 2 3 (4 7) 8 9 10 (11 14 17) 18 19 (20 23) 24 (25 28 31) 32 33 34 (35 38) (39 42 45) 46 47 48 49
    val adapterGroups2 = Vector(
      AdapterGroup(1, 1),
      AdapterGroup(2, 2),
      AdapterGroup(3, 3),
      AdapterGroup(4, 7),
      AdapterGroup(8, 8),
      AdapterGroup(9, 9),
      AdapterGroup(10, 10),
      AdapterGroup(11, 17),
      AdapterGroup(18, 18),
      AdapterGroup(19, 19),
      AdapterGroup(20, 23),
      AdapterGroup(24, 24),
      AdapterGroup(25, 31),
      AdapterGroup(32, 32),
      AdapterGroup(33, 33),
      AdapterGroup(34, 34),
      AdapterGroup(35, 38),
      AdapterGroup(39, 45),
      AdapterGroup(46, 46),
      AdapterGroup(47, 47),
      AdapterGroup(48, 48),
      AdapterGroup(49, 49)
    )
    Day10.groupAdapters(adapters2) should be(adapterGroups2)
  }

  "countPossibleAdapterArrangements" should "count the unique number of adapter arrangements to connect your device to the wall" in {
    val adapters1 = Vector(16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4)
    Day10.countPossibleAdapterArrangements(adapters1) should be(8)
    val adapters2 = Vector(28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3)
    Day10.countPossibleAdapterArrangements(adapters2) should be(1848)
  }
}
