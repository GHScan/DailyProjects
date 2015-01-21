package test.lexical

import java.io.ByteArrayInputStream

import lexical.{StreamCharSource, StringCharSource}
import org.scalatest._

class CharSourceTest extends FlatSpec with Matchers {

  behavior of "StringCharSource"
  it should "enable empty string" in {
    new StringCharSource("").toList should be(Nil)
  }
  it should "enable 'a'" in {
    new StringCharSource("a").toList should be("a".toList)
  }
  it should "enable 'abc'" in {
    new StringCharSource("abc").toList should be("abc".toList)
  }

  behavior of "StreamCharSource"
  it should "enable empty string" in {
    new StreamCharSource(new ByteArrayInputStream("".getBytes), 4).toList should be(Nil)
  }
  it should "enable 'a'" in {
    new StreamCharSource(new ByteArrayInputStream("a".getBytes), 4).toList should be("a".toList)
  }
  it should "enable 'abc'" in {
    new StreamCharSource(new ByteArrayInputStream("abc".getBytes), 4).toList should be("abc".toList)
  }
  it should "enable 'abcefghijklmn'" in {
    new StreamCharSource(new ByteArrayInputStream("abcefghijklmn".getBytes), 4).toList should be("abcefghijklmn".toList)
  }
  it should "enable string in length of 0~16" in {
    for (len <- 0 until 16) {
      val a = Array.fill(len)('a')
      new StreamCharSource(new ByteArrayInputStream(new String(a).getBytes), 4).toList should be(a.toList)
    }
  }
  it should "support rollback" in {
    val source = new StreamCharSource(new ByteArrayInputStream("abcefghijklmn".getBytes), 4)
    (source take 6 toList) should be("abcefg".toList)
    for (_ <- 0 until 6) source.rollback()
    source.toList should be("abcefghijklmn".toList)
    intercept[java.lang.AssertionError] {
      for (_ <- 0 until 9) source.rollback()
    }
    source.toList should be("jklmn".toList)
  }
}