package demo.json

import lexical.{TokenFactory, StringCharSource, TableDrivenScannerBuilder}
import parsing.{ParserFactory, GrammarBuilder}

object JsonParser {

  val ScannerBuilder = new TableDrivenScannerBuilder()
    .literals("[", "]", "{", "}", ":", ",")
    .token("WS", """\s""", _ => null)
    .token("BOOLEAN", "true|false", _ == "true")
    .token("NUMBER", """\d+(\.\d+)?""", _.toDouble)
    .token("STRING", """"(\\.|[^"])*"""", s => s.substring(1, s.length - 1))

  val Grammar = new GrammarBuilder {

    import ScannerBuilder.Implicits._
    import parsing._

    def start : INonTerminalSymbol = value

    val array : GenericNonTerminalSymbol[List[Any]] = nonTerm(
      "[" ~> value.repsep(",") <~ "]")
    val dict : GenericNonTerminalSymbol[Map[String, Any]] = nonTerm(
      "{" ~> ("STRING" ~ ":" ~ value ^^ { case key ~ _ ~ _value => (key.asInstanceOf[String], _value)}).repsep(",") <~ "}" ^^ (_.toMap))
    lazy val value : GenericNonTerminalSymbol[Any] = nonTerm(
      "BOOLEAN"
        | "NUMBER"
        | "STRING"
        | array
        | dict)

  }.result
}

class JsonParser(parserType : String) {
  private val parser = ParserFactory.get(parserType).create(JsonParser.Grammar, true)
  private val WS = JsonParser.ScannerBuilder.getToken("WS")

  def parse(input : String) : Any = {
    val scanner = JsonParser.ScannerBuilder.create(new StringCharSource(input), new TokenFactory).filter(_ != WS)
    val result = parser.parse(scanner)
    if (parser.errors != Nil) {
      val message = parser.errors.mkString("\n")
      parser.errors = Nil
      throw new Exception(message)
    } else {
      result
    }
  }
}
