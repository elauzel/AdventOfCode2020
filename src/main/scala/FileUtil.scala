import java.io.File
import scala.collection.mutable
import scala.io.Source

object FileUtil {
  def readResource(filename: String): Either[RuntimeException, Vector[String]] = {
    val file = new File(getClass.getClassLoader.getResource(filename).getPath)
    if (!file.exists() || file.isDirectory)
      Left(new RuntimeException(s"Failed to read ${file.getAbsolutePath}"))
    else try {
      val source = Source.fromFile(file)
      val lines = Right(source.getLines.toVector)
      source.close()
      lines
    } catch {
      case t: Throwable =>
        Left(new RuntimeException(s"Failed to read ${file.getAbsolutePath}: ${t.getMessage}"))
    }
  }

  def readResourceAsGroupedLines(filename: String): Either[RuntimeException, Vector[Vector[String]]] =
    readResource(filename).map { lines =>
      val groups = mutable.ListBuffer.empty[Vector[String]]
      var currentGroup = mutable.ListBuffer.empty[String]
      lines.foreach { line =>
        if (line.isEmpty) {
          groups += currentGroup.toVector
          currentGroup = mutable.ListBuffer.empty[String]
        } else currentGroup += line
      }
      groups.toVector :+ currentGroup.toVector
    }
}
