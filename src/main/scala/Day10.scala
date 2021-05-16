import scala.collection.parallel.CollectionConverters.ImmutableMapIsParallelizable
import scala.util.Try

object Day10 {
  private type Adapter = Int
  private val DeviceRating = 3
  private val MaxJoltageDifference = 3

  case class AdapterGroup(adapters: Adapter*) {
    def shrink: AdapterGroup = AdapterGroup(adapters.min, adapters.max) // just keep the smallest and biggest values in the group
  }

  def findJoltageDifferences(adapters: Vector[Adapter]): Map[Int, Int] =
    if (adapters.isEmpty)
      Map.empty[Int, Int]
    else {
      def go(adaptersAndWallAndDevice: Vector[Adapter]): Map[Int, Int] = {
        val differences = adaptersAndWallAndDevice
          .sliding(2)
          .map(pair => Math.abs(pair.head - pair.last))
          .toVector
        differences.distinct
          .map(difference => difference -> differences.count(_ == difference))
          .toMap
      }

      go(Vector(0) ++ adapters.sorted ++ Vector(adapters.max + DeviceRating)) // add the wall and the device
    }

  def findJoltageProduct(adapters: Vector[Adapter], groupBy: Vector[Int]): Int = {
    val joltageDifferenceCounts = findJoltageDifferences(adapters)
    groupBy.map(difference => Try(joltageDifferenceCounts(difference)).getOrElse(0)).product
  }

  def countPossibleAdapterArrangements(adapters: Vector[Adapter]): Int = {
    val device = adapters.max + MaxJoltageDifference
    val possibleConnections = findPossibleConnections(adapters).par

    def go(adapter: Adapter): Int =
      if (adapter == device)
        1
      else {
        possibleConnections(adapter).map(go)
      }.sum

    go(0)
  }

  def findPossibleConnections(adapters: Vector[Adapter]): Map[Adapter, Vector[Adapter]] = {
    val sorted = Vector(0) ++ adapters.sorted
    sorted
      .map(a => a -> Vector(a + 1, a + 2, a + 3).filter(sorted.contains))
      .toMap ++ Map(sorted.last -> Vector(sorted.last + MaxJoltageDifference))
  }

  def main(args: Array[String]): Unit =
    FileUtil.readResource("Day10.txt") match {
      case Left(exception) => throw exception
      case Right(lines) =>
        val adapters = lines.map(_.toInt)
        // Part 1 - Find a chain that uses all of your adapters to connect the charging outlet to your device's built-in adapter
        // and count the joltage differences between the charging outlet, the adapters, and your device.
        // What is the number of 1-jolt differences multiplied by the number of 3-jolt differences?
        val part1Answer = findJoltageProduct(adapters, Vector(1, 3))
        System.out.println(part1Answer)
        // Part 2 - What is the total number of distinct ways you can arrange the adapters to connect the charging outlet to your device?
        val part2Answer = countPossibleAdapterArrangements(adapters)
        System.out.println(part2Answer)
    }
}
