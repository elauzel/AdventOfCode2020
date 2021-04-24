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

  "isValidBirthYear" should " return true if byr is four digits; at least 1920 and at most 2002" in {
    Day04.isValidBirthYear("") should be(false)
    Day04.isValidBirthYear("error") should be(false)
    Day04.isValidBirthYear("1919") should be(false)
    Day04.isValidBirthYear("1920") should be(true)
    Day04.isValidBirthYear("2002") should be(true)
    Day04.isValidBirthYear("2003") should be(false)
  }

  "isValidIssueYear" should "return true if iyr is four digits; at least 2010 and at most 2020." in {
    Day04.isValidIssueYear("") should be(false)
    Day04.isValidIssueYear("error") should be(false)
    Day04.isValidIssueYear("2009") should be(false)
    Day04.isValidIssueYear("2010") should be(true)
    Day04.isValidIssueYear("2020") should be(true)
    Day04.isValidIssueYear("2021") should be(false)
  }

  "isValidExpirationYear" should "return true if eyr is four digits; at least 2020 and at most 2030." in {
    Day04.isValidExpirationYear("") should be(false)
    Day04.isValidExpirationYear("error") should be(false)
    Day04.isValidExpirationYear("2019") should be(false)
    Day04.isValidExpirationYear("2020") should be(true)
    Day04.isValidExpirationYear("2030") should be(true)
    Day04.isValidExpirationYear("2031") should be(false)
  }

  "isValidHeight" should "return true if the number is at least 150 and at most 193 for cm" in {
    Day04.isValidHeight("") should be(false)
    Day04.isValidHeight("error") should be(false)
    Day04.isValidHeight("149cm") should be(false)
    Day04.isValidHeight("150cm") should be(true)
    Day04.isValidHeight("150xx") should be(false)
    Day04.isValidHeight("193cm") should be(true)
    Day04.isValidHeight("194cm") should be(false)
  }

  "isValidHeight" should "return true if the number is at least 59 and at most 76 for in" in {
    Day04.isValidHeight("") should be(false)
    Day04.isValidHeight("error") should be(false)
    Day04.isValidHeight("58in") should be(false)
    Day04.isValidHeight("59in") should be(true)
    Day04.isValidHeight("59xx") should be(false)
    Day04.isValidHeight("76in") should be(true)
    Day04.isValidHeight("77in") should be(false)

  }

  "isValidHairColor" should "return true if a # followed by exactly six characters 0-9 or a-f" in {
    Day04.isValidHairColor("") should be(false)
    Day04.isValidHairColor("error") should be(false)
    Day04.isValidHairColor("abc123") should be(false)
    Day04.isValidHairColor("#abc12") should be(false)
    Day04.isValidHairColor("#abg123") should be(false)
    Day04.isValidHairColor("#abc123") should be(true)
    Day04.isValidHairColor("#abcdef") should be(true)
    Day04.isValidHairColor("#123456") should be(true)
    Day04.isValidHairColor("#abc1234") should be(false)
  }

  "isValidEyeColor" should "return true if color is exactly one of: amb blu brn gry grn hzl oth" in {
    Day04.isValidEyeColor("") should be(false)
    Day04.isValidEyeColor("xxx") should be(false)
    Day04.isValidEyeColor("error") should be(false)
    Day04.isValidEyeColor("amb") should be(true)
    Day04.isValidEyeColor("blu") should be(true)
    Day04.isValidEyeColor("brn") should be(true)
    Day04.isValidEyeColor("gry") should be(true)
    Day04.isValidEyeColor("grn") should be(true)
    Day04.isValidEyeColor("hzl") should be(true)
    Day04.isValidEyeColor("oth") should be(true)
  }

  "isValidPassportId" should "return true if it is a nine-digit number, including leading zeroes" in {
    Day04.isValidPassportId("") should be(false)
    Day04.isValidPassportId("error") should be(false)
    Day04.isValidPassportId("01234567") should be(false)
    Day04.isValidPassportId("0123456789") should be(false)
    Day04.isValidPassportId("012345678") should be(true)
    Day04.isValidPassportId("000045678") should be(true)
  }

  // Part 1

  "is passport with required fields" should "return true for lines containing all required fields (with cid optional)" in {
    val passport1 = "iyr:2010 ecl:gry hgt:181cm pid:591597745 byr:1920 hcl:#6b5442 eyr:2029 cid:123"
    val passport2 = "cid:223 byr:1927 hgt:177cm hcl:#602927 iyr:2016 pid:404183620 ecl:amb eyr:2020"
    val passport3 = "byr:1998 ecl:hzl cid:178 hcl:#a97842 iyr:2014 hgt:166cm pid:594143498 eyr:2030"
    val passport4 = "byr:1998 ecl:hzl hcl:#a97842 iyr:2014 hgt:166cm pid:594143498 eyr:2030"
    Day04.isPassportWithRequiredFields(passport1) should be(true)
    Day04.isPassportWithRequiredFields(passport2) should be(true)
    Day04.isPassportWithRequiredFields(passport3) should be(true)
    Day04.isPassportWithRequiredFields(passport4) should be(true)
  }

  "is passport with required fields" should "return false for lines missing any required field other than cid" in {
    Day04.isPassportWithRequiredFields("ecl:gry hgt:181cm pid:591597745 byr:1920 hcl:#6b5442 eyr:2029 cid:123") should be(false)
    Day04.isPassportWithRequiredFields("iyr:2010 hgt:181cm pid:591597745 byr:1920 hcl:#6b5442 eyr:2029 cid:123") should be(false)
    Day04.isPassportWithRequiredFields("iyr:2010 ecl:gry pid:591597745 byr:1920 hcl:#6b5442 eyr:2029 cid:123") should be(false)
    Day04.isPassportWithRequiredFields("iyr:2010 ecl:gry hgt:181cm byr:1920 hcl:#6b5442 eyr:2029 cid:123") should be(false)
    Day04.isPassportWithRequiredFields("iyr:2010 ecl:gry hgt:181cm pid:591597745 hcl:#6b5442 eyr:2029 cid:123") should be(false)
    Day04.isPassportWithRequiredFields("iyr:2010 ecl:gry hgt:181cm pid:591597745 byr:1920 eyr:2029 cid:123") should be(false)
    Day04.isPassportWithRequiredFields("iyr:2010 ecl:gry hgt:181cm pid:591597745 byr:1920 hcl:#6b5442 cid:123") should be(false)
  }

  // Part 2

  "is passport with required fields and valid data" should "return false for lines missing any required field (with cid optional) or having invalid data" in {
    Day04.isPassportWithRequiredFieldsAndValidData("ecl:gry hgt:181cm pid:591597745 byr:1920 hcl:#6b5442 eyr:2029 cid:123") should be(false)
    Day04.isPassportWithRequiredFieldsAndValidData("iyr:2010 hgt:181cm pid:591597745 byr:1920 hcl:#6b5442 eyr:2029 cid:123") should be(false)
    Day04.isPassportWithRequiredFieldsAndValidData("iyr:2010 ecl:gry pid:591597745 byr:1920 hcl:#6b5442 eyr:2029 cid:123") should be(false)
    Day04.isPassportWithRequiredFieldsAndValidData("iyr:2010 ecl:gry hgt:181cm byr:1920 hcl:#6b5442 eyr:2029 cid:123") should be(false)
    Day04.isPassportWithRequiredFieldsAndValidData("iyr:2010 ecl:gry hgt:181cm pid:591597745 hcl:#6b5442 eyr:2029 cid:123") should be(false)
    Day04.isPassportWithRequiredFieldsAndValidData("iyr:2010 ecl:gry hgt:181cm pid:591597745 byr:1920 eyr:2029 cid:123") should be(false)
    Day04.isPassportWithRequiredFieldsAndValidData("iyr:2010 ecl:gry hgt:181cm pid:591597745 byr:1920 hcl:#6b5442 cid:123") should be(false)

    val passport1 = "eyr:1972 cid:100 hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"
    val passport2 = "iyr:2019 hcl:#602927 eyr:1967 hgt:170cm ecl:grn pid:012533040 byr:1946"
    val passport3 = "hcl:dab227 iyr:2012 ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"
    val passport4 = "hgt:59cm ecl:zzz eyr:2038 hcl:74454a iyr:2023 pid:3556412378 byr:2007"
    Day04.isPassportWithRequiredFieldsAndValidData(passport1) should be(false)
    Day04.isPassportWithRequiredFieldsAndValidData(passport2) should be(false)
    Day04.isPassportWithRequiredFieldsAndValidData(passport3) should be(false)
    Day04.isPassportWithRequiredFieldsAndValidData(passport4) should be(false)
  }

  "is passport with required fields and valid data" should "return true for lines containing all required fields and valid data (with cid optional)" in {
    val passport1 = "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f"
    val passport2 = "eyr:2029 ecl:blu cid:129 byr:1989 iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
    val passport3 = "hcl:#888785 hgt:164cm byr:2001 iyr:2015 cid:88 pid:545766238 ecl:hzl eyr:2022"
    val passport4 = "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
    Day04.isPassportWithRequiredFieldsAndValidData(passport1) should be(true)
    Day04.isPassportWithRequiredFieldsAndValidData(passport2) should be(true)
    Day04.isPassportWithRequiredFieldsAndValidData(passport3) should be(true)
    Day04.isPassportWithRequiredFieldsAndValidData(passport4) should be(true)
  }
}
