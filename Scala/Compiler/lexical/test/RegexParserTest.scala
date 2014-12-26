package lexical.test

import org.scalatest._
import lexical.RegexParser
import lexical.RegexAST._
import utils.CharacterClass._

class RegexParserTest extends FlatSpec with Matchers {

  val p = new RegexParser()

  behavior of "Char parser"
  it should "parse literal" in {
    p.parse("""a""") should equal(Chars("a"))
    p.parse("""Z""") should equal(Chars("Z"))
    p.parse("""\n""") should equal(Chars("\n"))
    p.parse("""\t""") should equal(Chars("\t"))
    p.parse("""\\""") should equal(Chars("\\"))
  }
  it should "parse dot " in {
    p.parse(""".""") should equal(Chars(All))
  }
  it should "parse spaces" in {
    p.parse("""\s""") should equal(Chars(Spaces))
  }
  it should "parse digits" in {
    p.parse("""\d""") should equal(Chars(Digits))
  }
  it should "parse none letterDigits" in {
    p.parse("""\W""") should equal(Chars(NoneLetterOrDigits))
  }
  it should "parse range" in {
    p.parse("""[abcd]""") should equal(Chars("abcd"))
    p.parse("""[abcd0-9]""") should equal(Chars("abcd0123456789"))
  }

  behavior of "Kleene parser"
  it should "parse ?" in {
    p.parse("""a?""") should equal(Alternation(Chars("a"), Empty))
  }
  it should "parse +" in {
    p.parse("""a+""") should equal(KleenePlus(Chars("a")))
  }
  it should "parse *" in {
    p.parse("""a*""") should equal(Alternation(KleenePlus(Chars("a")), Empty))
  }

  behavior of "Concatenation parser"
  it should "parse xyz" in {
    p.parse("""a""") should equal(Chars("a"))
    p.parse("""ab""") should equal(Concatenation(Chars("a"), Chars("b")))
    p.parse("""abc""") should equal(Concatenation(Concatenation(Chars("a"), Chars("b")), Chars("c")))
  }

  behavior of "Alternation parser"
  it should "parse x|y|z" in {
    p.parse("""a""") should equal(Chars("a"))
    p.parse("""a|b""") should equal(Alternation(Chars("a"), Chars("b")))
    p.parse("""a|b|c""") should equal(Alternation(Alternation(Chars("a"), Chars("b")), Chars("c")))
    p.parse("""a1|b|c""") should equal(Alternation(Alternation(Concatenation(Chars("a"), Chars("1")), Chars("b")), Chars("c")))
  }

  behavior of "Complex parser"
  it should "parse complex re" in {
    p.parse("""([ab]|[\da]bc)*a""") should equal(
      Concatenation(Alternation(KleenePlus(
        Alternation(Chars("ab"),
          Concatenation(
            Concatenation(Chars("0123456789a"),
              Chars("b")),
            Chars("c")))),
        Empty), Chars("a")))
  }
}