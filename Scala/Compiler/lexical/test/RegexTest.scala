package lexical.test

import org.scalatest._
import lexical.NFARegex

class RegexTest extends FlatSpec with Matchers {
  behavior of "Prefix matcher"
  it should "match literal" in {
    val re = new NFARegex("abc")
    re.matchPrefix("abcdef") should equal("abc")
    re.matchPrefix("def") should equal("")
  }
  it should "match dot" in {
    val re = new NFARegex(".a")
    re.matchPrefix("za") should equal("za")
    re.matchPrefix("zba") should equal("")
  }
  it should "match longest" in {
    val re = new NFARegex("""123|\d+a""")
    re.matchPrefix("123456") should equal("123")
    re.matchPrefix("123456a") should equal("123456a")
  }

  behavior of "Full matcher"
  it should "match \\w+" in {
    val re = new NFARegex("""\w+""")
    re.isMatch("fsdjk234") should equal(true)
    re.isMatch("[[") should equal(false)
  }
}