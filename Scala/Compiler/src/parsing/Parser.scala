package parsing

trait IParser {
  var errors : List[String] = Nil
  def parse(scanner : Iterator[lexical.IToken]) : Any
}

trait IParserFactory {
  def create(grammar : Grammar) : IParser
}

object ParserFactory {
  val get = Map(
    "LL1" -> TableDrivenLL1ParserFactory,
    "LR0" -> TableDrivenLR0ParserFactory,
    "SLR" -> TableDrivenSLRParserFactory,
    "LALR" -> TableDrivenLALRParserFactory,
    "LR1" -> TableDrivenLR1ParserFactory
  )
}