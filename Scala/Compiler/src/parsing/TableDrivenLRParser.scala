package parsing

import scala.collection.mutable

final class TableDrivenLRParser(
  val name : String,
  val grammar : Grammar,
  val actionTable : ILRActionTable,
  val gotoTable : ILRGotoTable) extends LRParser {

  def recoverFromError(scanner : BufferedIterator[lexical.Token], _valueStack : List[Any], stateStack : mutable.Stack[Int]) : Option[List[Any]] = {

    var valueStack = _valueStack

    while (scanner.hasNext) {
      grammar.syncWord2ErrorRecoveryAction.get(scanner.head) match {
        case None => scanner.next()
        case Some(ErrorRecoveryAction(targetNonTerm, action, consumeSyncWord)) =>
          if (consumeSyncWord) scanner.next()
          while (stateStack.nonEmpty) {
            gotoTable.tryApply(stateStack.top, targetNonTerm) match {
              case None =>
                stateStack.pop()
                valueStack = valueStack.tail
              case Some(targetState) =>
                stateStack.push(targetState)
                return Some(action(null) :: valueStack)
            }
          }
      }
    }

    None
  }

  def parse(_scanner : Iterator[lexical.Token]) : Any = {
    val scanner = _scanner.buffered

    val stateStack = mutable.Stack[Int](0)
    var valueStack = List[Any]()

    try {
      while (stateStack.nonEmpty) {
        actionTable(stateStack.top, scanner.head.id) match {
          case LRAction.Shift(target) =>
            valueStack = scanner.next().value :: valueStack
            stateStack.push(target)
          case LRAction.Reduce(p) =>
            valueStack = p.action(valueStack)
            for (_ <- 0 until p.right.length) stateStack.pop()
            stateStack.push(gotoTable(stateStack.top, p.left.name))
          case LRAction.Accept(p) if scanner.head == lexical.Token.EOF =>
            valueStack = p.action(valueStack)
            return if (errors.isEmpty) valueStack.ensuring(_.length == 1).head else null
          case LRAction.Accept(p) =>
            valueStack = p.action(valueStack)
            for (_ <- 0 until p.right.length) stateStack.pop()
            stateStack.push(gotoTable(stateStack.top, p.left.name))
          case _ =>
            errors :+= s"Parse failed: found token ${scanner.head} in \n${scanner.head.locationText}"
            recoverFromError(scanner, valueStack, stateStack) match {
              case None => return null
              case Some(newValueStack) => valueStack = newValueStack
            }
        }
      }
    } catch {
      case e : Exception =>
        errors :+= e.toString
        return null
    }

    errors :+= s"Parse failed: input is too long, ${scanner.head}"
    null
  }
}
