import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try

object Day10 {
  private val DeviceRating = 3
  private val MaxJoltageDifference = 3

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
      .map(devices => devices.tail.filter(_ + MaxJoltageDifference >= devices.head).sum)
      .product
  }

  def findDistinctAdapterGroups(adapters: Vector[Int]): Vector[AdapterGroup] = {
    @tailrec
    def go(adaptersCurrentGroup: Vector[Int],
           adaptersRemaining: Vector[Int],
           previousPossibilityCount: Int,
           adapterGroups: Vector[AdapterGroup]): Vector[AdapterGroup] =
      if (adaptersRemaining.isEmpty) {
        val newGroup = AdapterGroup(adaptersCurrentGroup.min, adaptersCurrentGroup.max)
        adapterGroups :+ newGroup
      }
      else {
        // val adapters1 = Vector(16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4, 0)
        // (0 1 4) 5 6 (7 10) 11 (12 15 16 19)
        //   1 1  3 2 1  1  2   1   1  1  1  0
        val adapter = adaptersRemaining.head
        val possibleConnectionCount = adaptersRemaining.tail
          .takeWhile(_ <= adapter + MaxJoltageDifference)
          .size
        if (previousPossibilityCount == 1 && possibleConnectionCount == 1) {
          // add to current group
          go(adaptersCurrentGroup :+ adapter, adaptersRemaining.tail, possibleConnectionCount, adapterGroups)
        } else {
          // start new group
          if (adaptersCurrentGroup.isEmpty) {
            go(Vector(adapter), adaptersRemaining.tail, possibleConnectionCount, adapterGroups)
          } else {
            val newGroup = AdapterGroup(adaptersCurrentGroup.min, adapter)
            go(Vector.empty[Int], adaptersRemaining.tail, possibleConnectionCount, adapterGroups :+ newGroup)
          }
        }
      }

    go(Vector.empty[Int], Vector(0) ++ adapters.sorted, 0, Vector.empty[AdapterGroup])
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
