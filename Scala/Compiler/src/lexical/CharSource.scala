package lexical

import java.io.{InputStream, InputStreamReader}
import scala.io.Codec

trait CharSource extends Iterator[Char] {
  def rollback() : Unit
}

final class StringCharSource(str : String) extends CharSource {
  private var mOff = 0

  def hasNext : Boolean = mOff < str.length

  def next() : Char = {
    val c = str.charAt(mOff)
    mOff += 1
    c
  }

  def rollback() : Unit = {
    assert(mOff > 0)
    mOff -= 1
  }

}

final class StreamCharSource(inputStream : InputStream, bufferSize : Int = 4096)(implicit val codec : Codec) extends CharSource {

  private val mBufferSize = utils.Func.round2PowerOf2(bufferSize)
  private val mDBufferSize = mBufferSize * 2
  private val mDBufferMask = mDBufferSize - 1
  private val mReader = new InputStreamReader(inputStream, codec.decoder)
  private val mBuffer = new Array[Char](mDBufferSize)
  private var mOff = 0
  private var mStart = 0
  private var mEnd = 0
  private var mEof = -1

  readNextChunk()
  mStart = 0

  private def readNextChunk() {
    assert(hasNext)
    assert(mOff == 0 || mOff == mBufferSize)

    val readBytes = math.max(mReader.read(mBuffer, mOff, mBufferSize), 0)
    if (readBytes < mBufferSize) mEof = mOff + readBytes
    mEnd = (mOff + mBufferSize) & mDBufferMask
    mStart = (mOff - mBufferSize) & mDBufferMask
  }

  def hasNext : Boolean = mEof == -1 || mOff != mEof

  def next() : Char = {
    val c = mBuffer(mOff)
    mOff = (mOff + 1) & mDBufferMask
    if (mOff == mEnd) readNextChunk()
    c
  }

  def rollback() : Unit = {
    assert(mOff != mStart)
    mOff = (mOff - 1) & mDBufferMask
  }

}