package demo.json

import lexical.{IgnoreHandler, TokenFactory, StringCharSource, TableDrivenScannerBuilder}
import parsing.{ParserFactory, GrammarBuilder}

object JsonParser {

  val ScannerBuilder = new TableDrivenScannerBuilder()
    .literals("[", "]", "{", "}", ":", ",")
    .token("WS", """\s""", IgnoreHandler)
    .token("BOOLEAN", "true|false", _ == "true")
    .token("NUMBER", """\d+(\.\d+)?""", _.toDouble)
    .token("STRING", """"(\\.|[^"])*"""", s => s.substring(1, s.length - 1))

  val Grammar = new GrammarBuilder {

    import ScannerBuilder.Implicits._
    import parsing._

    def start : INonTerminalSymbol = value

    val array : GenericNonTerminalSymbol[List[Any]] = nonTerm(
      "[" ~> repsep(value, ",") <~ "]")
    val dict : GenericNonTerminalSymbol[Map[String, Any]] = nonTerm(
      "{" ~> repsep("STRING" ~ ":" ~ value ^^ { case key ~ _ ~ _value => (key.asInstanceOf[String], _value)}, ",") <~ "}" ^^ (_.toMap))
    lazy val value : GenericNonTerminalSymbol[Any] = nonTerm(
      "BOOLEAN" | "NUMBER" | "STRING" | array | dict)

    override def syncWord2ErrorRecoveryAction = Map[lexical.IToken, ErrorRecoveryAction]()
      .updated("}", ErrorRecoveryAction(value.name, _ => Map.empty, consumeSyncWord = true))

  }.result
}

class JsonParser(val name : String) {
  private val parser = ParserFactory.get(name).create(JsonParser.Grammar, reportConflict = true)

  def parse(input : String) : Any = {
    val result = parser.parse(JsonParser.ScannerBuilder.create(new StringCharSource(input), new TokenFactory))
    if (parser.errors.nonEmpty) throw new Exception(parser.errors.mkString("\n")) else result
  }
}
