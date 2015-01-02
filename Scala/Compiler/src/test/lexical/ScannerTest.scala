package test.lexical

import lexical._
import org.scalatest.{Matchers, FlatSpec}

class TableDrivenScannerTest extends FlatSpec with Matchers {
  behavior of "TokenBuilder"
  it should "pass simple test case" in {
    val tokenBuilder = new TableDrivenScannerBuilder()
      .token("WS", """[\t\n\r ]+""", _ => null)
      .token(",")
      .token(")")
      .token("(")
      .token("=")
      .token("+")
      .token("def")
      .token("INT", """\d+""", _.toInt)
      .token("IDENT", """\w+""", identity)

    val scanner = tokenBuilder.create(new StringCharSource("def func(abc, def) = println(abc, def + 1234)"), new TokenBuilder())
    val tokenList = List(
      Token("def"),
      TokenExt("IDENT", "func"), Token("("), TokenExt("IDENT", "abc"), Token(","), Token("def"), Token(")"),
      Token("="),
      TokenExt("IDENT", "println"), Token("("), TokenExt("IDENT", "abc"), Token(","), Token("def"), Token("+"), TokenExt("INT", 1234), Token(")"))
    scanner.filter(_.id != "WS").toList should equal(tokenList)
  }
}
