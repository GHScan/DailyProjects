import scala.util.Random
import scala.collection.mutable
import scala.collection.immutable

object Helper {

  def listFiles(dir : String, filter : scala.util.matching.Regex) : List[String] = {
    import java.io._

    def iterate(file : File) : List[String] = {
      if (file.isFile) {
        val path = file.getAbsolutePath
        filter.findFirstIn(path) match {
          case None => Nil
          case _ => List(path)
        }
      } else file.listFiles.toList.flatMap(iterate(_))
    }

    iterate(new File(dir))
  }

  def readFileLines(path : String) = {
    scala.io.Source.fromFile(path)(scala.io.Codec.ISO8859).getLines
  }

  def readFileBytes(path : String) : Array[Byte] = {
    import java.nio.file.Files;
    import java.nio.file.Paths;
    
    Files.readAllBytes(Paths.get(path))
  }

  def computeMd5(bytes : Array[Byte]) : String = {
    import java.security.MessageDigest
    
    MessageDigest.getInstance("MD5").digest(bytes).map(b => f"$b%02x").mkString
  }

  def mapWithFuture[T, U](a : List[T], f : T => U) : List[U] = {
    import scala.concurrent._
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global
    
    val future = a.map(v => Future { f(v) }).foldLeft(Future { List[U]() }) { (a, b) => a.flatMap(va => b.map(vb => vb :: va)) }
    Await.result(future, 30 seconds)
  }
}

object Test extends App {

  def main() {
    val kDir = raw"F:\Libraries\llvm-3.2.src"
    val kTimes = 3

    Utils.timeit("Count line (sync)", kTimes) {
      Helper.listFiles(kDir, """\.(c|cpp|h|hpp)$""".r) map (p => Helper.readFileLines(p).length) sum
    }
    Utils.timeit("Count line (parallel)", kTimes) {
      Helper.listFiles(kDir, """\.(c|cpp|h|hpp)$""".r).par map (p => Helper.readFileLines(p).length) sum
    }
    Utils.timeit("Count line (future)", kTimes) {
      Helper.mapWithFuture(
        Helper.listFiles(kDir, """\.(c|cpp|h|hpp)$""".r),
        ((p : String) => Helper.readFileLines(p).length)).sum
    }

    Utils.timeit("Compute Md5 (sync)", kTimes) {
      Helper.listFiles(kDir, """\.(c|cpp|h|hpp)$""".r) map (p => Helper.computeMd5(Helper.readFileBytes(p)))
    }
    Utils.timeit("Compute Md5 (parallel)", kTimes) {
      Helper.listFiles(kDir, """\.(c|cpp|h|hpp)$""".r).par map (p => Helper.computeMd5(Helper.readFileBytes(p)))
    }
    Utils.timeit("Compute Md5 (future)", kTimes) {
      Helper.mapWithFuture(
        Helper.listFiles(kDir, """\.(c|cpp|h|hpp)$""".r),
        ((p : String) => Helper.computeMd5(Helper.readFileBytes(p))))
    }
  }

  main()
}
