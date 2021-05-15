import scala.annotation.tailrec
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
    @tailrec
    def go(smallestGroup: AdapterGroup, largerGroups: Vector[AdapterGroup], countsByGroup: Seq[(AdapterGroup, Int)]): Map[AdapterGroup, Int] =
      if (largerGroups.isEmpty)
        countsByGroup.toMap
      else {
        val entry = smallestGroup -> largerGroups.takeWhile(_.adapters.min <= smallestGroup.adapters.max + MaxJoltageDifference).size
        go(largerGroups.head, largerGroups.tail, countsByGroup :+ entry)
      }

    val groups = Vector(AdapterGroup(0)) ++ findDistinctAdapterGroups(adapters.sorted) :+ AdapterGroup(adapters.max + DeviceRating)
    // adapters:  (16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4, 0)
    // sorted:    WALL (1 4) 5 6 (7 10) 11 (12 15 16 19) DEVICE
    // connections:   1     3 2 1      2  1             1

    // possibilities
    //
    // WALL (1 4) 5 6 (7 10) 11 (12 15 16 19) DEVICE
    // WALL (1 4) 5 6 (7 10)    (12 15 16 19) DEVICE
    // WALL (1 4) 5   (7 10) 11 (12 15 16 19) DEVICE
    // WALL (1 4) 5   (7 10)    (12 15 16 19) DEVICE
    // WALL (1 4)   6 (7 10) 11 (12 15 16 19) DEVICE
    // WALL (1 4)   6 (7 10)    (12 15 16 19) DEVICE
    // WALL (1 4)     (7 10) 11 (12 15 16 19) DEVICE
    // WALL (1 4)     (7 10)    (12 15 16 19) DEVICE
    val countsByGroup = go(groups.head, groups.tail, Seq.empty[(AdapterGroup, Int)])
    val product = countsByGroup.values.product
    System.out.println("do something with the above?")
    product // FIXME
  }

  def findDistinctAdapterGroups(adapters: Vector[Adapter]): Vector[AdapterGroup] = {
    @tailrec
    def go(adaptersCurrentGroup: Vector[Adapter],
           adaptersRemaining: Vector[Adapter],
           previousPossibleConnections: Int,
           adapterGroups: Vector[AdapterGroup]): Vector[AdapterGroup] =
      if (adaptersRemaining.isEmpty)
        adapterGroups
      else {
        val adapter = adaptersRemaining.head
        val currentPossibleConnections = adaptersRemaining.tail
          .takeWhile(_ <= adapter + MaxJoltageDifference)
          .size
        if (previousPossibleConnections < 2 && currentPossibleConnections == 1) {
          // add to current group
          go(adaptersCurrentGroup :+ adapter, adaptersRemaining.tail, currentPossibleConnections, adapterGroups)
        } else {
          // wrap up current group and start new group
          val newGroup =
            if (adaptersCurrentGroup.isEmpty) AdapterGroup(adapter)
            else AdapterGroup(adaptersCurrentGroup.min, adaptersCurrentGroup.max, adapter).shrink
          go(Vector.empty[Adapter], adaptersRemaining.tail, currentPossibleConnections, adapterGroups :+ newGroup)
        }
      }

    go(Vector.empty[Adapter], adapters.sorted, 0, Vector.empty[AdapterGroup])
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
