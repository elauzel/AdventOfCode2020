object Day07 {
  type BagColor = String

  case class Rule(bagColor: BagColor, allowedContents: Map[Int, BagColor])

  def parseRule(line: String): Rule =
    line match {
      case s"$bagColor bags contain no other bags." =>
        Rule(bagColor, Map.empty[Int, BagColor])
      case s"$bagColor bags contain $possibleContents." =>
        val allowedContents = possibleContents.split(',')
          .map(_.trim match {
            case s"$number $innerBagName bag$_" =>
              number.toInt -> innerBagName
          }).toMap
        Rule(bagColor, allowedContents)
      case _ =>
        throw new RuntimeException("line formatted incorrectly for rule parsing")
    }

  def findPossibleBagsToHold(bagColor: BagColor, rules: Set[Rule]): Set[BagColor] =
    findBags(bagColor, rules)

  private def findBags(bagColor: BagColor, rules: Set[Rule]): Set[BagColor] = {
    val newBags = rules
      .filter(_.allowedContents.values.exists(_ == bagColor))
      .map(_.bagColor)
    newBags ++ newBags.flatMap(color => findBags(color, rules))
  }

  def main(args: Array[String]): Unit =
    FileUtil.readResource("Day07.txt") match {
      case Left(exception) => throw exception
      case Right(lines) =>
        val rules = lines.map(parseRule).toSet
        // Part 1 - How many bag colors can, eventually, contain at least one shiny gold bag directly or indirectly?
        val part1Answer = findPossibleBagsToHold("shiny gold", rules).size
        System.out.println(part1Answer)
      // Part 2 - ???
      //        val part2Answer = ???
      //        System.out.println(part2Answer)
    }
}
