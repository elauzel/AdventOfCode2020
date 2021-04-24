import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day04Spec extends AnyFlatSpec with Matchers {
  "parse passport lines" should "return one line for each individual passport from the batch" in {
    val passport1Lines = Vector(
      "iyr:2010 ecl:gry hgt:181cm",
      "pid:591597745 byr:1920 hcl:#6b5442 eyr:2029 cid:123")
    val passport2Lines = Vector(
      "cid:223 byr:1927",
      "hgt:177cm hcl:#602927 iyr:2016 pid:404183620",
      "ecl:amb",
      "eyr:2020",
    )
    val passport3Lines = Vector(
      "byr:1998",
      "ecl:hzl",
      "cid:178 hcl:#a97842 iyr:2014 hgt:166cm pid:594143498 eyr:2030"
    )
    val allPassports = passport1Lines ++ Vector("") ++ passport2Lines ++ Vector("") ++ passport3Lines
    val passport1 = "iyr:2010 ecl:gry hgt:181cm pid:591597745 byr:1920 hcl:#6b5442 eyr:2029 cid:123"
    val passport2 = "cid:223 byr:1927 hgt:177cm hcl:#602927 iyr:2016 pid:404183620 ecl:amb eyr:2020"
    val passport3 = "byr:1998 ecl:hzl cid:178 hcl:#a97842 iyr:2014 hgt:166cm pid:594143498 eyr:2030"

    Day04.parsePassportLines(Vector.empty[String]) should be(Vector.empty[String])

    Day04.parsePassportLines(passport1Lines) should be(Vector(passport1))
    Day04.parsePassportLines(passport2Lines) should be(Vector(passport2))
    Day04.parsePassportLines(passport3Lines) should be(Vector(passport3))
    val passports1And2 = passport1Lines ++ Vector("") ++ passport2Lines
    Day04.parsePassportLines(passports1And2) should be(Vector(passport1, passport2))
    Day04.parsePassportLines(allPassports) should be(Vector(passport1, passport2, passport3))
  }

  // Part 1

  "is valid passport" should "return true for lines containing all required fields (with cid optional)" in {
    val passport1 = "iyr:2010 ecl:gry hgt:181cm pid:591597745 byr:1920 hcl:#6b5442 eyr:2029 cid:123"
    val passport2 = "cid:223 byr:1927 hgt:177cm hcl:#602927 iyr:2016 pid:404183620 ecl:amb eyr:2020"
    val passport3 = "byr:1998 ecl:hzl cid:178 hcl:#a97842 iyr:2014 hgt:166cm pid:594143498 eyr:2030"
    val passport4 = "byr:1998 ecl:hzl hcl:#a97842 iyr:2014 hgt:166cm pid:594143498 eyr:2030"
    Day04.isValidPassport(passport1) should be(true)
    Day04.isValidPassport(passport2) should be(true)
    Day04.isValidPassport(passport3) should be(true)
    Day04.isValidPassport(passport4) should be(true)
  }

  "is valid passport" should "return false for lines missing any required field other than cid" in {
    Day04.isValidPassport("ecl:gry hgt:181cm pid:591597745 byr:1920 hcl:#6b5442 eyr:2029 cid:123") should be(false)
    Day04.isValidPassport("iyr:2010 hgt:181cm pid:591597745 byr:1920 hcl:#6b5442 eyr:2029 cid:123") should be(false)
    Day04.isValidPassport("iyr:2010 ecl:gry pid:591597745 byr:1920 hcl:#6b5442 eyr:2029 cid:123") should be(false)
    Day04.isValidPassport("iyr:2010 ecl:gry hgt:181cm byr:1920 hcl:#6b5442 eyr:2029 cid:123") should be(false)
    Day04.isValidPassport("iyr:2010 ecl:gry hgt:181cm pid:591597745 hcl:#6b5442 eyr:2029 cid:123") should be(false)
    Day04.isValidPassport("iyr:2010 ecl:gry hgt:181cm pid:591597745 byr:1920 eyr:2029 cid:123") should be(false)
    Day04.isValidPassport("iyr:2010 ecl:gry hgt:181cm pid:591597745 byr:1920 hcl:#6b5442 cid:123") should be(false)
  }
}
