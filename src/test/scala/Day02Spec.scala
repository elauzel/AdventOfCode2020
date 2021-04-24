import Day02.{PasswordAndOccurrancePolicy, PasswordAndPositionPolicy}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day02Spec extends AnyFlatSpec with Matchers {
  "password meets company occurrance policy" should "return true if the specified char occurs min ≤ n ≤ max times in the password" in {
    Day02.passwordMeetsCompanyOccurrancePolicy(PasswordAndOccurrancePolicy(1, 3, 'a', "abc")) should be(true)
    Day02.passwordMeetsCompanyOccurrancePolicy(PasswordAndOccurrancePolicy(1, 3, 'a', "aac")) should be(true)
    Day02.passwordMeetsCompanyOccurrancePolicy(PasswordAndOccurrancePolicy(1, 3, 'a', "aaa")) should be(true)
  }

  "password meets company occurrance policy" should "return false if the specified char occurs < min or > max times in the password" in {
    Day02.passwordMeetsCompanyOccurrancePolicy(PasswordAndOccurrancePolicy(1, 3, 'a', "bc")) should be(false)
    Day02.passwordMeetsCompanyOccurrancePolicy(PasswordAndOccurrancePolicy(1, 3, 'a', "aaaa")) should be(false)
  }

  "parse password and occurrance policy" should "parse a character, the minimum and maximum times it can occur, and a password, from a line" in {
    Day02.parsePasswordAndOccurrancePolicy("1-3 a: abcde") should be(PasswordAndOccurrancePolicy(1, 3, 'a', "abcde"))
    Day02.parsePasswordAndOccurrancePolicy("1-3 b: cdefg") should be(PasswordAndOccurrancePolicy(1, 3, 'b', "cdefg"))
    Day02.parsePasswordAndOccurrancePolicy("2-9 c: ccccccccc") should be(PasswordAndOccurrancePolicy(2, 9, 'c', "ccccccccc"))
  }

  "password meets company position policy" should "return true if the specified char occurs exactly once at each specified index in the password" in {
    Day02.passwordMeetsCompanyPositionPolicy(PasswordAndPositionPolicy(1, 3, 'a', "abc")) should be(true)
    Day02.passwordMeetsCompanyPositionPolicy(PasswordAndPositionPolicy(1, 3, 'a', "cba")) should be(true)
  }

  "password meets company position policy" should "return true if the specified char does not occurs exactly once at each specified index in the password" in {
    Day02.passwordMeetsCompanyPositionPolicy(PasswordAndPositionPolicy(1, 3, 'a', "baca")) should be(false)
    Day02.passwordMeetsCompanyPositionPolicy(PasswordAndPositionPolicy(1, 3, 'a', "abac")) should be(false)
  }

  "parse password and position policy" should "parse a character, the first and second index at which it can occur, and a password, from a line" in {
    Day02.parsePasswordAndPositionPolicy("1-3 a: abcde") should be(PasswordAndPositionPolicy(1, 3, 'a', "abcde"))
    Day02.parsePasswordAndPositionPolicy("1-3 b: cdefg") should be(PasswordAndPositionPolicy(1, 3, 'b', "cdefg"))
    Day02.parsePasswordAndPositionPolicy("2-9 c: ccccccccc") should be(PasswordAndPositionPolicy(2, 9, 'c', "ccccccccc"))
  }

  // Part 1

  "count passwords meeting company occurrance policy" should "count how many lines have passwords that meet the contained company policy" in {
    val lines = Vector("1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc")
    Day02.countPasswordsMeetingOccurrancePolicy(lines) should be(2)
  }

  // Part 2

  "count passwords meeting company position policy" should "count how many lines have passwords that meet the contained company policy" in {
    val lines = Vector("1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc")
    Day02.countPasswordsMeetingPositionPolicy(lines) should be(1)
  }
}
