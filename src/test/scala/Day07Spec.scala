import Day07.Rule
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day07Spec extends AnyFlatSpec with Matchers {
  "parse rule" should "parse a bag that contains no other bags" in {
    val rule = Rule("striped lavender", Map.empty[Int, String])
    Day07.parseRule("striped lavender bags contain no other bags.") should be(rule)
  }

  "parse rule" should "parse a bag that contains one other bag" in {
    val rule = Rule("striped lavender", Map(1 -> "pale lavender"))
    Day07.parseRule("striped lavender bags contain 1 pale lavender bag.") should be(rule)
  }

  "parse rule" should "parse a bag that contains several other bags" in {
    val rule = Rule("striped lavender", Map(3 -> "pale lavender", 4 -> "muted cyan"))
    Day07.parseRule("striped lavender bags contain 3 pale lavender bags, 4 muted cyan bags.") should be(rule)
  }

  // Part 1

  "find possible bags to hold" should "find all bag colors which can directly or indirectly hold another bag color" in {
    val rules = Set(
      Rule("red", Map.empty[Int, String]),
      Rule("orange", Map(1 -> "red")),
      Rule("yellow", Map(1 -> "red", 1 -> "orange")),
      Rule("green", Map(1 -> "red", 1 -> "orange", 1 -> "yellow")),
      Rule("blue", Map(1 -> "orange", 1 -> "yellow", 1 -> "green")),
      Rule("indigo", Map(1 -> "yellow", 1 -> "green", 1 -> "blue")),
      Rule("violet", Map(1 -> "green", 1 -> "blue", 1 -> "indigo")),
    )

    Day07.findPossibleBagsToHold("red", rules) should be(Set("orange", "yellow", "green", "blue", "indigo", "violet"))
    Day07.findPossibleBagsToHold("orange", rules) should be(Set("yellow", "green", "blue", "indigo", "violet"))
    Day07.findPossibleBagsToHold("yellow", rules) should be(Set("green", "blue", "indigo", "violet"))
    Day07.findPossibleBagsToHold("green", rules) should be(Set("blue", "indigo", "violet"))
    Day07.findPossibleBagsToHold("blue", rules) should be(Set("indigo", "violet"))
    Day07.findPossibleBagsToHold("indigo", rules) should be(Set("violet"))
    Day07.findPossibleBagsToHold("violet", rules) should be(Set.empty[String])
  }
}
