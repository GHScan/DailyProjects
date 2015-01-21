package parsing

import scala.collection.immutable

final class TableDrivenLLBacktrackingParser(_grammar : Grammar) extends TableDrivenLLParser(_grammar) {

  def name : String = TableDrivenLLBacktrackingParserFactory.name

  private var error : String = ""

  def parse(_scanner : Iterator[lexical.Token]) : Any = {

    def parseSymbols(symbols : List[IGrammarSymbol], scanner : List[lexical.Token], valueStack : List[Any]) : Stream[(List[lexical.Token], List[Any])] = symbols match {
      case Nil => Stream((scanner, valueStack))
      case head :: tail => parseSymbol(head, scanner, valueStack).flatMap { case (scanner2, valueStack2) =>
        parseSymbols(tail, scanner2, valueStack2)
      }
    }
    def parseSymbol(symbol : IGrammarSymbol, scanner : List[lexical.Token], valueStack : List[Any]) : Stream[(List[lexical.Token], List[Any])] = symbol match {
      case t : TerminalSymbol if scanner.head == t.token =>
        Stream((scanner.tail, scanner.head.value :: valueStack))
      case t : TerminalSymbol =>
        error = s"Parse failed: expected $t, but found ${scanner.head}"
        Stream.empty
      case nt : INonTerminalSymbol =>
        val table = nt.context.asInstanceOf[Array[List[IProduction]]]
        val ps = table(scanner.head.id)
        if (ps == Nil) {
          error = s"Parse failed: miss predicate while parsing $nt, ${scanner.head}"
          return Stream.empty
        }
        ps.toStream.flatMap { p =>
          parseSymbols(p.right, scanner, valueStack).map { case (scanner2, valueStack2) => (scanner2, p.action(valueStack2))}
        }
    }

    parseSymbol(grammar.start, _scanner.toList, Nil).find(_._1.head == lexical.Token.EOF) match {
      case Some((_, valueStack)) if valueStack.length == 1 =>
        valueStack.head
      case None =>
        errors = immutable.Queue[String](error)
        null
    }
  }
}

object TableDrivenLLBacktrackingParserFactory extends IParserFactory {
  def ll : Boolean = true
  def name : String = "LLBacktracking"
  def create(grammar : Grammar, reportConflict : Boolean) : IParser = new TableDrivenLLBacktrackingParser(grammar)
}