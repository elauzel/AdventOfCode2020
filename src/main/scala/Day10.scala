import scala.collection.mutable
import scala.util.Try

object Day10 {
  private val DeviceRating = 3

  case class AdapterGroup(first: Int, last: Int)

  def findJoltageDifferences(adapters: Vector[Int]): Map[Int, Int] =
    if (adapters.isEmpty)
      Map.empty[Int, Int]
    else {
      def go(adaptersAndWallAndDevice: Vector[Int]): Map[Int, Int] = {
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

  def findJoltageProduct(adapters: Vector[Int], groupBy: Vector[Int]): Int = {
    val joltageDifferenceCounts = findJoltageDifferences(adapters)
    groupBy.map(difference => Try(joltageDifferenceCounts(difference)).getOrElse(0)).product
  }

  def countPossibleAdapterArrangements(adapters: Vector[Int]): Int = {
    val devicesHiLow = (Vector(0) ++ adapters.sorted ++ Vector(adapters.max + DeviceRating)).reverse
    devicesHiLow.sliding(4)
      .map(devices => devices.tail.filter(_ + 3 >= devices.head).sum)
      .product
  }

  def groupAdapters(adapters: Vector[Int], currentIndex: Int = 0): Vector[AdapterGroup] = {
    val sorted = adapters.sorted
    //    val possibleSteps = mutable.ListBuffer.from(adapters.map(_ => 0))
    val possibleStepsForEachAdapter = sorted
      .indices
      .map { index =>
        val adapter = sorted(index)
        val nextAdapters = sorted.slice(index + 1, index + 4)
        val count = nextAdapters.count(_ + 3 >= adapter)
        count
      }

    Vector.empty[AdapterGroup]
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
      //        val part2Answer = part1Answer
      //          .flatMap(findContiguousNumbers(numbers, _))
      //          .map(findEncryptionWeakness)
      //        System.out.println(part2Answer)
    }
}
