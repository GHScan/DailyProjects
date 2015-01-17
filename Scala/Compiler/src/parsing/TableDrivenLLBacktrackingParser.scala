package parsing


final class TableDrivenLLBacktrackingParser(_grammar : Grammar) extends LLParser(_grammar) {
  def name : String = TableDrivenLLBacktrackingParserFactory.name
  def parse(_scanner : Iterator[lexical.IToken]) : Any = {

    def parseSymbols(symbols : List[IGrammarSymbol], scanner : List[lexical.IToken], valueStack : List[Any]) : Stream[(List[lexical.IToken], List[Any])] = symbols match {
      case Nil => Stream((scanner, valueStack))
      case head :: tail => parseSymbol(head, scanner, valueStack).flatMap { case (scanner, valueStack) =>
        parseSymbols(tail, scanner, valueStack)
      }
    }
    def parseSymbol(symbol : IGrammarSymbol, scanner : List[lexical.IToken], valueStack : List[Any]) : Stream[(List[lexical.IToken], List[Any])] = symbol match {
      case t : TerminalSymbol if scanner.head == t.token =>
        Stream((scanner.tail, scanner.head.value :: valueStack))
      case t : TerminalSymbol =>
        errors = s"Parse failed: expected $t, but found ${scanner.head}" :: errors
        Stream.empty
      case nt : INonTerminalSymbol =>
        val table = nt.context.asInstanceOf[Array[List[IProduction]]]
        val ps = table(scanner.head.id)
        if (ps == Nil) {
          errors = s"Parse failed: miss predicate while parsing $nt, ${scanner.head}" :: errors
          return Stream.empty
        }
        ps.toStream.flatMap { p =>
          parseSymbols(p.right, scanner, valueStack).map { case (scanner, valueStack) => (scanner, p.action(valueStack))}
        }
    }

    parseSymbol(grammar.start, _scanner.toList, Nil).find(_._1.head == lexical.IToken.Eof) match {
      case Some((_, valueStack)) if valueStack.length == 1 =>
        errors = Nil
        valueStack.head
      case None => null
    }
  }
}

object TableDrivenLLBacktrackingParserFactory extends IParserFactory {
  def ll : Boolean = true
  def name : String = "LLBacktracking"
  def create(grammar : Grammar, reportConflict : Boolean) : IParser = new TableDrivenLLBacktrackingParser(grammar)
}