package parsing

final class TableDrivenGLRParser(
  val name : String,
  val grammar : Grammar,
  val actionTable : ILRActionTable[List[LRAction.Action]],
  val gotoTable : ILRGotoTable) extends LRParser[List[LRAction.Action]] {

  private sealed abstract class Context
  private case class Process(stateStack : List[Int], scanner : List[lexical.IToken], valueStack : List[Any]) extends Context
  private case class Success(value : Any) extends Context

  def parse(_scanner : Iterator[lexical.IToken]) : Any = {

    def deriveContext(stateStack : List[Int], scanner : List[lexical.IToken], valueStack : List[Any]) : List[Context] = actionTable(stateStack.head, scanner.head.id).map {
      case LRAction.Shift(target) =>
        Process(target :: stateStack, scanner.tail, scanner.head.value :: valueStack)
      case LRAction.Reduce(p) =>
        val stateStack2 = stateStack.drop(p.right.length)
        Process(gotoTable(stateStack2.head, p.left.name) :: stateStack2, scanner, p.action(valueStack))
      case LRAction.Accept(p) if scanner.head == lexical.IToken.EOF =>
        Success(p.action(valueStack).ensuring(_.length == 1).head)
      case LRAction.Accept(p) =>
        val stateStack2 = stateStack.drop(p.right.length)
        Process(gotoTable(stateStack2.head, p.left.name) :: stateStack2, scanner, p.action(valueStack))
    }

    def iterate(contexts : List[Context]) : Any = contexts match {
      case Nil =>
        errors :+= "Parser failed..."
        null
      case Success(value) :: tail =>
        value
      // GLR requires bread-first search, but i prefer depth-first here
      case Process(stateStack, scanner, valueStack) :: tail =>
        iterate(deriveContext(stateStack, scanner, valueStack) ::: tail)
    }

    iterate(List(Process(List(0), _scanner.toList, Nil)))
  }
}
