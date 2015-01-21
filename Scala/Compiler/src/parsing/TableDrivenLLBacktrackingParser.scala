package parsing

final class TableDrivenLLBacktrackingParser(_grammar : Grammar) extends TableDrivenLLParser(_grammar) {

  def name : String = TableDrivenLLBacktrackingParserFactory.name

  private case class DummyProductionSymbol(production : IProduction) extends IGrammarSymbol

  private sealed abstract class Context
  private case class Process(symbolStack : List[IGrammarSymbol], scanner : List[lexical.IToken], valueStack : List[Any]) extends Context
  private case class Success(value : Any) extends Context

  def parse(_scanner : Iterator[lexical.IToken]) : Any = {

    def deriveContext(symbolStack : List[IGrammarSymbol], scanner : List[lexical.IToken], valueStack : List[Any]) : List[Context] = symbolStack match {
      case Nil if scanner.head == lexical.IToken.EOF =>
        List(Success(valueStack.ensuring(_.length == 1).head))
      case Nil => Nil
      case (t : TerminalSymbol) :: tail if t.token == scanner.head =>
        List(Process(tail, scanner.tail, scanner.head.value :: valueStack))
      case (t : TerminalSymbol) :: tail => Nil
      case (nt : INonTerminalSymbol) :: tail =>
        val table = nt.context.asInstanceOf[Array[List[IProduction]]]
        table(scanner.head.id).map { p =>
          Process(p.right ::: DummyProductionSymbol(p) :: tail, scanner, valueStack)
        }
      case DummyProductionSymbol(p) :: tail =>
        List(Process(tail, scanner, p.action(valueStack)))
    }

    def iterate(contexts : List[Context]) : Any = contexts match {
      case Nil =>
        errors :+= "Parse failed..."
        null
      case Success(value) :: tail =>
        value
      case Process(symbolStack, scanner, valueStack) :: tail =>
        iterate(deriveContext(symbolStack, scanner, valueStack) ::: tail)
    }

    iterate(List(Process(List(grammar.start), _scanner.toList, Nil)))

  }
}

object TableDrivenLLBacktrackingParserFactory extends IParserFactory {
  def ll : Boolean = true
  def name : String = "LLBacktracking"
  def create(grammar : Grammar, reportConflict : Boolean) : IParser = new TableDrivenLLBacktrackingParser(grammar)
}