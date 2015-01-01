package lexical

import java.io.{InputStream, InputStreamReader}
import scala.io.Codec

trait ICharSource extends Iterator[Char] {
  def rollback() : Unit
}

final class StringCharSource(str : String) extends ICharSource {
  private var off = 0

  def hasNext : Boolean = off < str.length

  def next() : Char = {
    val c = str.charAt(off)
    off += 1
    c
  }

  def rollback() : Unit = {
    assert(off > 0)
    off -= 1
  }

}

final class StreamCharSource(inputStream : InputStream, bufferSize : Int = 4096)(implicit val codec : Codec) extends ICharSource {

  private val BUFF_SIZE = utils.Func.round2PowerOf2(bufferSize)
  private val DBUFF_SIZE = BUFF_SIZE * 2
  private val DBUFF_MASK = DBUFF_SIZE - 1
  private val reader = new InputStreamReader(inputStream, codec.decoder)
  private val buffer = new Array[Char](DBUFF_SIZE)
  private var off = 0
  private var start = 0
  private var end = 0
  private var eof = -1

  readNextChunk()
  start = 0

  private def readNextChunk() {
    assert(hasNext)
    assert(off == 0 || off == BUFF_SIZE)

    val readBytes = math.max(reader.read(buffer, off, BUFF_SIZE), 0)
    if (readBytes < BUFF_SIZE) eof = off + readBytes
    end = (off + BUFF_SIZE) & DBUFF_MASK
    start = (off - BUFF_SIZE) & DBUFF_MASK
  }

  def hasNext : Boolean = eof == -1 || off != eof

  def next() : Char = {
    val c = buffer(off)
    off = (off + 1) & DBUFF_MASK
    if (off == end) readNextChunk()
    c
  }

  def rollback() : Unit = {
    assert(off != start)
    off = (off - 1) & DBUFF_MASK
  }

}