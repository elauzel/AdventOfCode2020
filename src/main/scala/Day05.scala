object Day05 {
  private type Row = Int
  private type Column = Int
  private type SeatID = Int
  private val MaxRow = 127
  private val MaxColumn = 7

  private def binarySpaceSearch(line: String, m: Int, lowerHalfChar: Char, upperHalfChar: Char): Int = {
    var range = 0 until m + 1
    line.foreach { c =>
      if (c == lowerHalfChar)
        range = range.splitAt(range.size / 2)._1
      else if (c == upperHalfChar)
        range = range.splitAt(range.size / 2)._2
    }
    range.min // min and max are equal here
  }

  def determineSeatRow(line: String): Row = binarySpaceSearch(line, MaxRow, 'F', 'B')

  def determineSeatColumn(line: String): Column = binarySpaceSearch(line, MaxColumn, 'L', 'R')

  def determineSeatRowColumn(line: String): (Row, Column) =
    (determineSeatRow(line.take(7)), determineSeatColumn(line.drop(7)))

  def determineSeatId(row: Row, column: Column): SeatID = row * 8 + column

  def determineMissingSeats(foundSeats: Vector[(Row, Column)],
                            maxRow: Row = MaxRow,
                            maxColumn: Column = MaxColumn): Seq[(Row, Column)] = {
    val allSeats =
      for {row <- 0 to maxRow
           column <- 0 to maxColumn
           } yield (row, column)
    allSeats.diff(foundSeats)
  }

  def findMissingSeatBetweenTwoFoundSeats(foundSeats: Vector[(Row, Column)],
                                          maxRow: Row = MaxRow,
                                          maxColumn: Column = MaxColumn): Seq[(Row, Column)] = {
    val foundSeatIds = foundSeats.map { rowColumn =>
      determineSeatId(rowColumn._1, rowColumn._2)
    }
    determineMissingSeats(foundSeats, maxRow, maxColumn)
      .filter { seat =>
        val row = seat._1
        if (row != 0 && row != maxRow) {
          val column = seat._2
          val seatId = determineSeatId(row, column)
          foundSeatIds.contains(seatId + 1) && foundSeatIds.contains(seatId - 1)
        } else false
      }
  }

  def main(args: Array[String]): Unit =
    FileUtil.readResource("Day05.txt") match {
      case Left(exception) => throw exception
      case Right(lines) =>
        val foundSeats = lines.map(determineSeatRowColumn)
        // Part 1 - What is the highest seat ID on a boarding pass?
        val foundSeatIds = foundSeats.map { rowColumn =>
          determineSeatId(rowColumn._1, rowColumn._2)
        }
        val part1Answer = foundSeatIds.max
        System.out.println(part1Answer)
        // Part 2 - What is your seat ID given that your seat wasn't at the very front or back of the plane, and is between two occupied seats?
        val part2Answer = findMissingSeatBetweenTwoFoundSeats(foundSeats)
          .map(seat => determineSeatId(seat._1, seat._2))
          .head
        System.out.println(part2Answer)
    }
}
