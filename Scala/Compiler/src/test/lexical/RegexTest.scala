package test.lexical

import lexical.{DFARegex, NFARegex, Regex, RegexParser}
import org.scalatest._

class NFARegexTest extends FlatSpec with Matchers {
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
    val re = new NFARegex( """123|\d+a""")
    re.matchPrefix("123456") should equal("123")
    re.matchPrefix("123456a") should equal("123456a")
  }

  behavior of "Full matcher"
  it should "match \\w+" in {
    val re = new NFARegex( """\w+""")
    re.isMatch("fsdjk234") should equal(true)
    re.isMatch("[[") should equal(false)
  }

}

class DFARegexTest extends FlatSpec with Matchers {
  behavior of "Prefix matcher"
  it should "match literal" in {
    val re = new DFARegex("abc")
    re.matchPrefix("abcdef") should equal("abc")
    re.matchPrefix("def") should equal("")
  }
  it should "match dot" in {
    val re = new DFARegex(".a")
    re.matchPrefix("za") should equal("za")
    re.matchPrefix("zba") should equal("")
  }
  it should "match longest" in {
    val re = new DFARegex( """123|\d+a""")
    re.matchPrefix("123456") should equal("123")
    re.matchPrefix("123456a") should equal("123456a")
  }

  behavior of "Full matcher"
  it should "match \\w+" in {
    val re = new DFARegex( """\w+""")
    re.isMatch("fsdjk234") should equal(true)
    re.isMatch("[[") should equal(false)
  }

}

class RegexPatternEqualityTest extends FlatSpec with Matchers {
  behavior of "Regex Pattern"
  it should "enable test for equality" in {
    Regex.patternEquals(
      "0|1|2|3|4|5|6|7|8|9|10|11|12|13|14|15",
      "0|1[0-5]?|[2-9]") should be(right = true)
    Regex.patternEquals(
      "here|there|this|that",
      "(he|the)re|th(at|is)") should be(right = true)
  }
}

class RegexSimplifyTest extends FlatSpec with Matchers {
  behavior of "Regex"
  it should "enable simplify" in {
    new RegexParser().parse("0|1|2|3|4|5|6|7|8|9|10|11|12|13|14|15").simplified.toPattern should be("\\d|1[0-5]")
  }

}