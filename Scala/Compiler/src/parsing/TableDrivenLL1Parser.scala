package parsing

import scala.collection.mutable

final class TableDrivenLL1Parser(_grammar : Grammar) extends TableDrivenLLParser(_grammar) {

  def name : String = TableDrivenLL1ParserFactory.name

  def recoverFromError(scanner : BufferedIterator[lexical.Token], _valueStack : List[Any], symbolStack : mutable.Stack[Any]) : Option[List[Any]] = {

    var valueStack = _valueStack

    while (scanner.hasNext) {
      grammar.syncWord2ErrorRecoveryAction.get(scanner.head) match {
        case None => scanner.next()
        case Some(ErrorRecoveryAction(targetNonTerm, action, consumeSyncWord)) =>
          if (consumeSyncWord) scanner.next()
          while (symbolStack.nonEmpty) {
            symbolStack.pop() match {
              case p : IProduction if p.left.name == targetNonTerm => return Some(action(null) :: valueStack)
              case p : IProduction =>
              case _ => valueStack = valueStack.tail
            }
          }
      }
    }

    None
  }

  def parse(_scanner : Iterator[lexical.Token]) : Any = {
    if (!predictable) {
      throw new Exception(s"Grammar is not predictable!\n Grammar=$grammar\n\nParser=$this")
    }

    val scanner = _scanner.buffered

    var valueStack = List[Any]()
    val symbolStack = mutable.Stack[Any](grammar.start)

    try {
      while (symbolStack.nonEmpty) {
        symbolStack.pop() match {
          case t : TerminalSymbol if t.token == scanner.head =>
            valueStack = scanner.next().value :: valueStack
          case t : TerminalSymbol =>
            errors :+= s"Parse failed: Expected $t, but found ${scanner.head} in \n${scanner.head.locationText}"
            recoverFromError(scanner, valueStack, symbolStack) match {
              case None => return null
              case Some(newValueStack) => valueStack = newValueStack
            }
          case nt : INonTerminalSymbol =>
            val table = nt.context.asInstanceOf[Array[List[IProduction]]]
            val ps = table(scanner.head.id)
            if (ps == Nil) {
              errors :+= s"Parse failed: miss predicate while parsing $nt, ${scanner.head} in \n${scanner.head.locationText}"
              recoverFromError(scanner, valueStack, symbolStack) match {
                case None => return null
                case Some(newValueStack) => valueStack = newValueStack
              }
            } else {
              val p = ps.head
              symbolStack.push(p)
              symbolStack.pushAll(p.right.reverseIterator)
            }
          case p : IProduction =>
            valueStack = p.action(valueStack)
        }
      }
    } catch {
      case e : Exception =>
        errors :+= e.toString
        return null
    }

    if (scanner.head != lexical.Token.EOF) {
      errors :+= "Parse failed: input is too long!"
      return null
    }

    if (errors.isEmpty) valueStack.ensuring(_.length == 1).head else null
  }
}

object TableDrivenLL1ParserFactory extends IParserFactory {
  def ll : Boolean = true
  def name : String = "LL1"
  def create(grammar : Grammar, reportConflict : Boolean) : IParser = new TableDrivenLL1Parser(grammar)
}