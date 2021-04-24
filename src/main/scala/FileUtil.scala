import java.io.File
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
}
