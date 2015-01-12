package parsing

import scala.collection.mutable

abstract class LLParser(_grammar : Grammar) extends IParser {

  val grammar = _grammar.removeLeftRecursion().leftFactoring()

  private def bindPredicateArray() : Unit = {
    for (nt <- grammar.nonTerms) {
      nt.context = Array.tabulate(grammar.maxTermID + 1) { tid =>
        nt.productions.filter(grammar.firstPlusMap(_).contains(tid))
      }
    }
  }
  bindPredicateArray()

  val predictable = grammar.nonTerms.forall(nt => nt.context.asInstanceOf[Array[List[IProduction]]].forall(_.length <= 1))

  override def toString =
    s"""predictable=$predictable\n${
      (for (nt <- grammar.nonTerms)
      yield s"\t$nt: (predictable=${nt.context.asInstanceOf[Array[List[IProduction]]].forall(_.length <= 1)})\n" +
          s"${nt.productions.map(p => s"\t\t$p:\n\t\t\t${grammar.firstPlusMap(p).map(grammar.id2Term).mkString(",")}").mkString("\n")}").mkString("\n\n")
    }
     """
}

final class TableDrivenLL1Parser(_grammar : Grammar) extends LLParser(_grammar) {

  def parse(_scanner : Iterator[lexical.IToken]) : Any = {
    assert(predictable)

    val scanner = _scanner.buffered

    val valueStack = mutable.Stack[Any]()
    val symbolStack = mutable.Stack[Any](grammar.start)

    while (symbolStack.nonEmpty) {
      symbolStack.pop() match {
        case t : TerminalSymbol if t == TerminalSymbol.EMPTY =>
          valueStack.push(null)
        case t : TerminalSymbol if t.token == scanner.head =>
          valueStack.push(scanner.next())
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
          p.action(valueStack)
      }
    }

    if (scanner.head != TerminalSymbol.EOF.token) {
      errors = "Parse failed: input is too long!" :: errors
      return null
    }

    valueStack.ensuring(_.length == 1).pop()
  }
}

object TableDrivenLL1ParserFactory extends IParserFactory {
  def create(grammar : Grammar) : IParser = new TableDrivenLL1Parser(grammar)
}