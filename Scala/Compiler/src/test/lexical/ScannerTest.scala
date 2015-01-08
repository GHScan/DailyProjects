package test.lexical

import lexical._
import org.scalatest.{Matchers, FlatSpec}

class TableDrivenScannerTest extends FlatSpec with Matchers {
  behavior of "TableDrivenScanner"
  it should "pass simple test case" in {
    val scannerBuilder = new TableDrivenScannerBuilder()
      .token("ws", """[\t\n\r ]+""", _ => null)
      .token(",")
      .token(")")
      .token("(")
      .token("=")
      .token("+")
      .token("def")
      .token("int", """\d+""", _.toInt)
      .token("ident", """\w+""", identity)

    implicit def str2Token(name : String) : IToken = scannerBuilder.lookupToken(name)

    val scanner = scannerBuilder.create(new StringCharSource("def func(abc, def) = println(abc, def + 1234)"), new TokenFactory())
    val tokenList = List[IToken](
      "def",
      "ident", "(", "ident", ",", "def", ")",
      "=",
      "ident", "(", "ident", ",", "def", "+", "int", ")")
    scanner.filter(_ != ("ws" : IToken)).toList should equal(tokenList)
  }
}
