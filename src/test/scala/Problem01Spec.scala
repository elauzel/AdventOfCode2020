import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Problem01Spec extends AnyFlatSpec with Matchers {
  "Problem 01" should "find the product of the two entries that sum to a desired number" in {
    val numbers = Vector(1721, 979, 366, 299, 675, 1456)
    Problem01.findProductOfTwoSummedNumbers(2020, numbers) should be(Some(1721 * 299))
  }
  "Problem 01" should "find no product if there are fewer than two numbers" in {
    val numbers = Vector(2020)
    Problem01.findProductOfTwoSummedNumbers(2020, numbers) should be(None)
    Problem01.findProductOfTwoSummedNumbers(2020, Vector.empty[Int]) should be(None)
  }
  "Problem 01" should "find no product if there are no two entries that sum to a desired number" in {
    val numbers = Vector(1, 2, 3)
    Problem01.findProductOfTwoSummedNumbers(6, numbers) should be(None)
  }
}
