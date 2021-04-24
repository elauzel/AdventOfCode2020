import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Problem01Spec extends AnyFlatSpec with Matchers {
  // Part 1

  "find product of two summed numbers" should "find the product of the two entries that sum to a desired number" in {
    val numbers = Vector[Long](1721, 979, 366, 299, 675, 1456)
    Problem01.findProductOfTwoSummedNumbers(2020, numbers) should be(Right(1721 * 299))
  }

  "find product of two summed numbers" should "find no product if there are fewer than two numbers" in {
    val numbers = Vector[Long](2020)
    Problem01.findProductOfTwoSummedNumbers(2020, numbers).isLeft should be(true)
    Problem01.findProductOfTwoSummedNumbers(2020, Vector.empty[Long]).isLeft should be(true)
  }

  "find product of two summed numbers" should "find no product if there are no two entries that sum to a desired number" in {
    val numbers = Vector[Long](1, 2, 3)
    Problem01.findProductOfTwoSummedNumbers(6, numbers).isLeft should be(true)
  }

  // Part 2

  "find product of three summed numbers" should "find the product of the three entries that sum to a desired number" in {
    val numbers = Vector[Long](1721, 979, 366, 299, 675, 1456)
    Problem01.findProductOfThreeSummedNumbers(2020, numbers) should be(Right(979 * 366 * 675))
  }

  "find product of three summed numbers" should "find no product if there are fewer than three numbers" in {
    val numbers = Vector[Long](1, 2019)
    Problem01.findProductOfThreeSummedNumbers(2020, numbers).isLeft should be(true)
    Problem01.findProductOfThreeSummedNumbers(2020, Vector.empty[Long]).isLeft should be(true)
  }

  "find product of three summed numbers" should "find no product if there are no three entries that sum to a desired number" in {
    val numbers = Vector[Long](1, 2, 3)
    Problem01.findProductOfThreeSummedNumbers(7, numbers).isLeft should be(true)
  }
}
