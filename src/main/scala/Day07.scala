object Day07 {
  type BagColor = String

  case class Rule(bagColor: BagColor, allowedContents: Map[BagColor, Int])

  def parseRule(line: String): Rule =
    line match {
      case s"$bagColor bags contain no other bags." =>
        Rule(bagColor, Map.empty[BagColor, Int])
      case s"$bagColor bags contain $possibleContents." =>
        val allowedContents = possibleContents.split(',')
          .map(_.trim match {
            case s"$number $innerBagName bag$_" =>
              innerBagName -> number.toInt
          }).toMap
        Rule(bagColor, allowedContents)
      case _ =>
        throw new RuntimeException("line formatted incorrectly for rule parsing")
    }

  def findPossibleBagsToHold(bagColor: BagColor, rules: Set[Rule]): Set[BagColor] = {
    val newBags = rules
      .filter(_.allowedContents.keys.exists(_ == bagColor))
      .map(_.bagColor)
    newBags ++ newBags.flatMap(color => findPossibleBagsToHold(color, rules))
  }

  def countNumberOfBagsHeld(bagColor: BagColor, rules: Set[Rule]): Int = {
    def go(bagColor: BagColor, rules: Set[Rule]): Int = {
      val rule = rules.filter(_.bagColor == bagColor).head
      1 + rule.allowedContents.map(kvp => kvp._2 * go(kvp._1, rules)).sum
    }

    go(bagColor, rules) - 1
  }

  def main(args: Array[String]): Unit =
    FileUtil.readResource("Day07.txt") match {
      case Left(exception) => throw exception
      case Right(lines) =>
        val rules = lines.map(parseRule).toSet
        // Part 1 - How many bag colors can, eventually, contain at least one shiny gold bag directly or indirectly?
        val part1Answer = findPossibleBagsToHold("shiny gold", rules).size
        System.out.println(part1Answer)
        // Part 2 - How many individual bags are required inside your single shiny gold bag?
        val part2Answer = countNumberOfBagsHeld("shiny gold", rules)
        System.out.println(part2Answer)
    }
}
