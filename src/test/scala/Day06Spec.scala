import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day06Spec extends AnyFlatSpec with Matchers {
  // Part 1
  "count questions affirmed by anyone" should "count how many distinct questions were answered yes by anyone in the group" in {
    Day06.countQuestionsAffirmedByAnyone(Vector("abc")) should be(3)
    Day06.countQuestionsAffirmedByAnyone(Vector("a", "b", "c")) should be(3)
    Day06.countQuestionsAffirmedByAnyone(Vector("ab", "ac")) should be(3)
    Day06.countQuestionsAffirmedByAnyone(Vector("a", "a", "a", "a")) should be(1)
    Day06.countQuestionsAffirmedByAnyone(Vector("b")) should be(1)
  }

  // Part 2

  "count questions affirmed by everyone" should "count how many distinct questions were answered yes by everyone in the group" in {
    Day06.countQuestionsAffirmedByEveryone(Vector("abc")) should be(3)
    Day06.countQuestionsAffirmedByEveryone(Vector("a", "b", "c")) should be(0)
    Day06.countQuestionsAffirmedByEveryone(Vector("ab", "ac")) should be(1)
    Day06.countQuestionsAffirmedByEveryone(Vector("a", "a", "a", "a")) should be(1)
    Day06.countQuestionsAffirmedByEveryone(Vector("b")) should be(1)
  }
}
