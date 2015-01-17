package parsing

import scala.collection.mutable

final class TableDrivenLL1Parser(_grammar : Grammar) extends LLParser(_grammar) {
  def name : String = TableDrivenLL1ParserFactory.name
  def parse(_scanner : Iterator[lexical.IToken]) : Any = {
    if (!predictable) {
      throw new Exception(s"Grammar is not predictable!\n Grammar=$grammar\n\nParser=$this")
    }

    val scanner = _scanner.buffered

    var valueStack = List[Any]()
    val symbolStack = mutable.Stack[Any](grammar.start)

    while (symbolStack.nonEmpty) {
      symbolStack.pop() match {
        case t : TerminalSymbol if t.token == scanner.head =>
          valueStack = scanner.next().value :: valueStack
        case t : TerminalSymbol =>
          errors = s"Parse failed: Expected $t, but found ${scanner.head}" :: errors
          return null
        case nt : INonTerminalSymbol =>
          val table = nt.context.asInstanceOf[Array[List[IProduction]]]
          val ps = table(scanner.head.id)
          if (ps == Nil) {
            errors = s"Parse failed: miss predicate while parsing $nt, ${scanner.head}" :: errors
            return null
          }
          val p = ps.head
          symbolStack.push(p)
          symbolStack.pushAll(p.right.reverseIterator)
        case p : IProduction =>
          valueStack = p.action(valueStack)
      }
    }

    if (scanner.head != lexical.IToken.Eof) {
      errors = "Parse failed: input is too long!" :: errors
      return null
    }

    valueStack.ensuring(_.length == 1).head
  }
}

object TableDrivenLL1ParserFactory extends IParserFactory {
  def ll : Boolean = true
  def name : String = "LL1"
  def create(grammar : Grammar, reportConflict : Boolean) : IParser = new TableDrivenLL1Parser(grammar)
}