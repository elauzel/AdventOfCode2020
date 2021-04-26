import Day07.Rule
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day07Spec extends AnyFlatSpec with Matchers {
  "parse rule" should "parse a bag that contains no other bags" in {
    val rule = Rule("striped lavender", Map.empty[String, Int])
    Day07.parseRule("striped lavender bags contain no other bags.") should be(rule)
  }

  "parse rule" should "parse a bag that contains one other bag" in {
    val rule = Rule("striped lavender", Map("pale lavender" -> 1))
    Day07.parseRule("striped lavender bags contain 1 pale lavender bag.") should be(rule)
  }

  "parse rule" should "parse a bag that contains several other bags" in {
    val rule = Rule("striped lavender", Map("pale lavender" -> 3, "muted cyan" -> 4))
    Day07.parseRule("striped lavender bags contain 3 pale lavender bags, 4 muted cyan bags.") should be(rule)
  }

  // Part 1

  "find possible bags to hold" should "find all bag colors which can directly hold another bag color" in {
    val rules = Set(
      Rule("orange", Map("red" -> 1, "etc" -> 1)),
      Rule("yellow", Map("red" -> 1, "etc" -> 1)),
      Rule("green", Map("red" -> 1, "etc" -> 1)),
      Rule("blue", Map("red" -> 1, "etc" -> 1)),
      Rule("indigo", Map("red" -> 1, "etc" -> 1)),
      Rule("violet", Map("red" -> 1, "etc" -> 1))
    )

    Day07.findPossibleBagsToHold("red", rules) should be(Set("orange", "yellow", "green", "blue", "indigo", "violet"))
  }

  "find possible bags to hold" should "find all bag colors which can indirectly hold another bag color" in {
    val rules = Set(
      Rule("red", Map.empty[String, Int]),
      Rule("orange", Map("red" -> 1)),
      Rule("yellow", Map("orange" -> 1)),
      Rule("green", Map("yellow" -> 1)),
      Rule("blue", Map("green" -> 1)),
      Rule("indigo", Map("blue" -> 1)),
      Rule("violet", Map("indigo" -> 1)),
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
