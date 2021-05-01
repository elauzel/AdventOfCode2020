import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day09Spec extends AnyFlatSpec with Matchers {
  "findFirstNumberWithoutSum" should "return the first number that is not the sum of two previous x numbers, where x is the preamble" in {
    val numbers = Vector[Long](35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576)
    Day09.findFirstNumberWithoutSum(numbers, 5) should be(Some(127))
  }
  "findFirstNumberWithoutSum" should "return no number when none are the sum of two previous x numbers, where x is the preamble" in {
    val numbers = Vector[Long](1, 2, 3, 4, 5, 6, 7, 8, 9)
    Day09.findFirstNumberWithoutSum(numbers, 1) should be(Some(2))
    Day09.findFirstNumberWithoutSum(numbers, 2) should be(Some(4))
    Day09.findFirstNumberWithoutSum(numbers, 3) should be(Some(6))
    Day09.findFirstNumberWithoutSum(numbers, 4) should be(Some(8))
    Day09.findFirstNumberWithoutSum(numbers, 5) should be(None)
    Day09.findFirstNumberWithoutSum(numbers, 6) should be(None)
    Day09.findFirstNumberWithoutSum(numbers, 7) should be(None)
    Day09.findFirstNumberWithoutSum(numbers, 8) should be(None)
  }

  // Part 2

  "findContiguousNumbers" should "find a sequence of at least two numbers that have the desired sum" in {
    val numbers = Vector[Long](35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576)
    Day09.findContiguousNumbers(numbers, 127) should be(Some(Vector(15, 25, 47, 40)))
  }

  // Part 2

  "findEncryptionWeakness" should "finds the encryption weakness using a sequence of contiguous numbers" in {
    Day09.findEncryptionWeakness(Vector(9, 3, 6, 2, 6, 8, 1)) should be(10)
    Day09.findEncryptionWeakness(Vector(9, 4, 1)) should be(10)
    Day09.findEncryptionWeakness(Vector(9, 1)) should be(10)
    Day09.findEncryptionWeakness(Vector(9)) should be(18)
  }
}
