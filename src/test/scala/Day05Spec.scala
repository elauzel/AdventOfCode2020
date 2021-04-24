import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day05Spec extends AnyFlatSpec with Matchers {
  "determine seat id" should "multiply the row by 8 and then add the column" in {
    Day05.determineSeatId(0, 5) should be(5)
    Day05.determineSeatId(1, 5) should be(13)
    Day05.determineSeatId(2, 5) should be(21)
    Day05.determineSeatId(3, 5) should be(29)
    Day05.determineSeatId(3, 0) should be(24)
  }

  "determine seat row" should "use binary space partitioning to find the row, F keeps lower half B keeps upper half" in {
    Day05.determineSeatRow("FBFBBFF") should be(44)
    Day05.determineSeatRow("FFFFFFF") should be(0)
    Day05.determineSeatRow("FFFFFFB") should be(1)
    Day05.determineSeatRow("FFFFFBB") should be(3)
    Day05.determineSeatRow("FFFFBBB") should be(7)
    Day05.determineSeatRow("FFFBBBB") should be(15)
    Day05.determineSeatRow("FFBBBBB") should be(31)
    Day05.determineSeatRow("FBBBBBB") should be(63)
    Day05.determineSeatRow("BBBBBBB") should be(127)
  }

  "determine seat column" should "use binary space partitioning to find the column, L keeps lower half R keeps upper half" in {
    Day05.determineSeatColumn("RLR") should be(5)
    Day05.determineSeatColumn("LLL") should be(0)
    Day05.determineSeatColumn("LLR") should be(1)
    Day05.determineSeatColumn("LRR") should be(3)
    Day05.determineSeatColumn("RRR") should be(7)
  }

  "determine seat row/ column" should "use binary space partitioning to find the row column, F/L keeps lower half B/R keeps upper half" in {
    Day05.determineSeatRowColumn("FFFFFFFLLL") should be((0, 0))
    Day05.determineSeatRowColumn("FBFBBFFRLR") should be((44, 5))
    Day05.determineSeatRowColumn("BBBBBBBRRR") should be((127, 7))
  }

  // Part 2

  "determine missing seats" should "determine which seat/row combinations are missing from those passed" in {
    val missingSeats = Day05.determineMissingSeats(Vector((0, 2), (1, 1), (1, 2)), 2, 2)
    missingSeats should be(Vector((0, 0), (0, 1), (1, 0), (2, 0), (2, 1), (2, 2)))
  }

  "find missing seat between two found seats" should "find the missing seat whose id is between the id of two found seats" in {
    val foundSeats = Vector(
      (0, 0), (0, 1), (0, 2), (0, 3),
      (1, 0), (1, 1), (1, 2), (1, 3),
      (2, 0), (2, 1),         (2, 3),
      (3, 0), (3, 1), (3, 2), (3, 3)
    )
    Day05.findMissingSeatBetweenTwoFoundSeats(foundSeats, 3, 3) should be(Vector((2, 2)))
  }
}
