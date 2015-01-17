package parsing

import scala.collection.mutable

final class TableDrivenLRParser(val name : String, val actionTable : ILRActionTable, val gotoTable : ILRGotoTable) extends LRParser {
  def parse(_scanner : Iterator[lexical.IToken]) : Any = {
    val scanner = _scanner.buffered

    val stateStack = mutable.Stack[Int](0)
    var valueStack = List[Any]()

    while (stateStack.nonEmpty) {
      actionTable(stateStack.top, scanner.head.id) match {
        case LRAction.Shift(target) =>
          valueStack = scanner.next().value :: valueStack
          stateStack.push(target)
        case LRAction.Reduce(p) =>
          valueStack = p.action(valueStack)
          for (_ <- 0 until p.right.length) stateStack.pop()
          stateStack.push(gotoTable(stateStack.top, p.left.name))
        case LRAction.Accept(p) if scanner.head == lexical.IToken.Eof =>
          valueStack = p.action(valueStack)
          return valueStack.ensuring(_.length == 1).head
        case LRAction.Accept(p) =>
          valueStack = p.action(valueStack)
          for (_ <- 0 until p.right.length) stateStack.pop()
          stateStack.push(gotoTable(stateStack.top, p.left.name))
        case _ =>
          errors = s"Parse failed: found token ${scanner.head}" :: errors
          return null
      }
    }

    errors = s"Parse failed: input is too long, ${scanner.head}" :: errors
    null
  }
}
