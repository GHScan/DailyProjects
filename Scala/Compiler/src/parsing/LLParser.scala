package parsing

abstract class LLParser(_grammar : Grammar) extends IParser {
  val grammar = _grammar.removeLeftRecursion().leftFactoring()
}