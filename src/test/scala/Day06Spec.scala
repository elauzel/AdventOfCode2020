import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day06Spec extends AnyFlatSpec with Matchers {
  "count questions answered per group" should "count how many distinct questions were answered yes by the group" in {
    Day06.countQuestionsAnsweredPerGroup(Vector("abc")) should be(3)
    Day06.countQuestionsAnsweredPerGroup(Vector("a", "b", "c")) should be(3)
    Day06.countQuestionsAnsweredPerGroup(Vector("ab", "ac")) should be(3)
    Day06.countQuestionsAnsweredPerGroup(Vector("a", "a", "a", "a")) should be(1)
    Day06.countQuestionsAnsweredPerGroup(Vector("b")) should be(1)
  }
}
