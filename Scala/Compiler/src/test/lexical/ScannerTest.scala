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

  def createScanner(input : String) = scannerBuilder.create(new StringCharSource(input), new FastTokenFactory())

  import scannerBuilder.Implicits._

  behavior of "TableDrivenScanner"

  it should "Enable Error input" in {
    val scanner = createScanner("123 abc <>")
    val expectTokens = List[Token](
      "int", "ident", Token.ERROR)
    scanner.toList should equal(expectTokens)
  }

  it should "Pass simple test case" in {
    val scanner = createScanner("def func(abc, def) = println(abc, def + 1234)")
    val expectTokens = List[Token](
      "def",
      "ident", "(", "ident", ",", "def", ")",
      "=",
      "ident", "(", "ident", ",", "def", "+", "int", ")", Token.EOF)
    scanner.toList should equal(expectTokens)
  }
}
