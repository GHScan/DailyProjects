package test.lexical

import lexical.{Token, TokenBuilder, StringCharSource, TableDrivenScannerBuilder}
import org.scalatest.{Matchers, FlatSpec}

class TableDrivenScannerTest extends FlatSpec with Matchers {
  behavior of "TokenBuilder"
  it should "pass simple test case" in {
    val tokenBuilder = new TableDrivenScannerBuilder()
      .token("WS", """[\t\n\r ]+""", _ => null)
      .token(",", """,""")
      .token(")", """\)""")
      .token("(", """\(""")
      .token("=", """=""")
      .token("+", """\+""")
      .token("def", """def""")
      .token("INT", """\d+""", _.toInt)
      .token("IDENT", """\w+""")

    val scanner = tokenBuilder.create(new StringCharSource("def func(abc, def) = println(abc, def + 1234)"), new TokenBuilder())
    val tokenList = List(
      Token("def", "def"),
      Token("IDENT", "func"), Token("(", "("), Token("IDENT", "abc"), Token(",", ","), Token("def", "def"), Token(")", ")"),
      Token("=", "="),
      Token("IDENT", "println"), Token("(", "("), Token("IDENT", "abc"), Token(",", ","), Token("def", "def"), Token("+", "+"), Token("INT", 1234), Token(")", ")"))
    scanner.filter(_.id != "WS").toList should equal(tokenList)
  }
}
