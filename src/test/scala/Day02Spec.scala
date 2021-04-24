import Day02.PasswordAndPolicy
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day02Spec extends AnyFlatSpec with Matchers {
  "password meets company policy" should "return true if the specified char occurs min ≤ n ≤ max times in the password" in {
    Day02.passwordMeetsCompanyPolicy(PasswordAndPolicy(1, 3, 'a', "abc")) should be(true)
    Day02.passwordMeetsCompanyPolicy(PasswordAndPolicy(1, 3, 'a', "aac")) should be(true)
    Day02.passwordMeetsCompanyPolicy(PasswordAndPolicy(1, 3, 'a', "aaa")) should be(true)
  }

  "password meets company policy" should "return false if the specified char occurs < min or > max times in the password" in {
    Day02.passwordMeetsCompanyPolicy(PasswordAndPolicy(1, 3, 'a', "bc")) should be(false)
    Day02.passwordMeetsCompanyPolicy(PasswordAndPolicy(1, 3, 'a', "aaaa")) should be(false)
  }

  "parse password and policy" should "parse a character, the minimum and maximum times it can occur, and a password, from a line" in {
    Day02.parsePasswordAndPolicy("1-3 a: abcde") should be(PasswordAndPolicy(1, 3, 'a', "abcde"))
    Day02.parsePasswordAndPolicy("1-3 b: cdefg") should be(PasswordAndPolicy(1, 3, 'b', "cdefg"))
    Day02.parsePasswordAndPolicy("2-9 c: ccccccccc") should be(PasswordAndPolicy(2, 9, 'c', "ccccccccc"))
  }

  // Part 1

  "count passwords meeting company policy" should "count how many lines have passwords that meet the contained company policy" in {
    val lines = Vector("1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc")
    Day02.countPasswordsMeetingCompanyPolicy(lines) should be(2)
  }

  // Part 2

  // TODO
}
