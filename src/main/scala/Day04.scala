import scala.collection.mutable
import scala.util.Try

object Day04 {
  private val RequiredPassportFields = Vector("ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt") // cid is optional
  private val ValidEyeColors = Vector("amb", "blu", "brn", "gry", "grn", "hzl", "oth")

  def isPassportWithRequiredFields(line: String): Boolean = {
    val passportData = line.split(" ")
      .map(entry => {
        val keyAndValue = entry.split(":")
        keyAndValue.head -> keyAndValue.last
      }).toMap
    RequiredPassportFields.forall(passportData.contains)
  }

  def isValidBirthYear(byr: String): Boolean =
    Try {
      val int = byr.toInt
      1919 < int && int < 2003
    }.getOrElse(false)

  def isValidIssueYear(iyr: String): Boolean =
    Try {
      val int = iyr.toInt
      2009 < int && int < 2021
    }.getOrElse(false)

  def isValidExpirationYear(eyr: String): Boolean =
    Try {
      val int = eyr.toInt
      2019 < int && int < 2031
    }.getOrElse(false)

  def isValidHeight(hgt: String): Boolean =
    hgt match {
      case s"${number}cm" =>
        val int = number.toInt
        149 < int && int < 194
      case s"${number}in" =>
        val int = number.toInt
        58 < int && int < 77
      case _ => false
    }

  def isValidHairColor(hcl: String): Boolean = hcl.matches("^[#][0-9a-f]{6}$")

  def isValidEyeColor(ecl: String): Boolean = ValidEyeColors.contains(ecl)

  def isValidPassportId(pid: String): Boolean = pid.matches("^[0-9]{9}$")

  def isPassportWithRequiredFieldsAndValidData(line: String): Boolean =
    Try {
      val passportData = line.split(" ")
        .map(entry => {
          val keyAndValue = entry.split(":")
          keyAndValue.head -> keyAndValue.last
        }).toMap

      val validEcl = isValidEyeColor(passportData("ecl"))
      val validPid = isValidPassportId(passportData("pid"))
      val validEyr = isValidExpirationYear(passportData("eyr"))
      val validHcl = isValidHairColor(passportData("hcl"))
      val validByr = isValidBirthYear(passportData("byr"))
      val validIyr = isValidIssueYear(passportData("iyr"))
      val validHgt = isValidHeight(passportData("hgt"))

      validEcl && validPid && validEyr && validHcl && validByr && validIyr && validHgt
    }.getOrElse(false)

  def parsePassportLines(lines: Vector[String]): Vector[String] = {
    val passports = mutable.ListBuffer.empty[String]
    var currentPassport = mutable.ListBuffer.empty[String]
    lines.foreach { line =>
      if (line.isEmpty) {
        passports += currentPassport.mkString(" ")
        currentPassport = mutable.ListBuffer.empty[String]
      } else currentPassport += line
    }
    (passports += currentPassport.mkString(" ")).filter(_.nonEmpty).toVector
  }

  def main(args: Array[String]): Unit =
    FileUtil.readResource("Day04.txt") match {
      case Left(exception) => throw exception
      case Right(lines) =>
        // Part 1 - How many passports are valid after modifying the scanner to ignore cid?
        val part1Answer = parsePassportLines(lines)
          .map(isPassportWithRequiredFields)
          .count(b => b)
        System.out.println(part1Answer)
        // Part 2 - How many passports are valid after modifying the scanner to ignore cid and adding data validation?
        val part2Answer = parsePassportLines(lines)
          .map(isPassportWithRequiredFieldsAndValidData)
          .count(b => b)
        System.out.println(part2Answer)
    }
}
