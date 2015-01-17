package test.lexical

import lexical._
import org.scalatest.{Matchers, FlatSpec}

class TableDrivenScannerTest extends FlatSpec with Matchers {

  val scannerBuilder = new TableDrivenScannerBuilder()
    .token("ws", """[\t\n\r ]+""", IgnoreHandler)
    .token(",")
    .token(")")
    .token("(")
    .token("=")
    .token("+")
    .token("def")
    .token("int", """\d+""", _.toInt)
    .token("ident", """\w+""", identity)

  def createScanner(input : String) = scannerBuilder.create(new StringCharSource(input), new TokenFactory())

  import scannerBuilder.Implicits._

  behavior of "TableDrivenScanner"

  it should "Enable Error input" in {
    val scanner = createScanner("123 abc <>")
    val expectTokens = List[IToken](
      "int", "ident", IToken.ERROR)
    scanner.toList should equal(expectTokens)
  }

  it should "Pass simple test case" in {
    val scanner = createScanner("def func(abc, def) = println(abc, def + 1234)")
    val expectTokens = List[IToken](
      "def",
      "ident", "(", "ident", ",", "def", ")",
      "=",
      "ident", "(", "ident", ",", "def", "+", "int", ")", IToken.EOF)
    scanner.toList should equal(expectTokens)
  }
}
