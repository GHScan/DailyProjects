package parsing

import scala.collection.mutable

abstract class TableDrivenGLRParserBuilder extends ILRParserBuilder[List[LRAction.Action]] {

  protected def create(
    transitions : Array[List[(IGrammarSymbol, Int)]],
    id2Set : Array[List[LRItem]],
    item2LookAheads : (Int, LRItem) => List[TerminalSymbol]) : TableDrivenGLRParser = {

    val actionTable = Array.fill(id2Set.size, grammar.maxTermID + 1)(Nil : List[LRAction.Action])
    val gotoTable = Array.fill(id2Set.size)(mutable.HashMap[String, Int]())

    for ((trans, id) <- transitions.zipWithIndex) {
      val gotoRow = gotoTable(id)
      for ((nt : INonTerminalSymbol, target) <- trans) gotoRow(nt.name) = target

      val reduces = for (item <- id2Set(id).filter(_.complete);
                         action = if (item.production.left == grammar.start) LRAction.Accept(item.production) else LRAction.Reduce(item.production);
                         term <- item2LookAheads(id, item)) yield (term, action)
      val shifts = trans.collect { case (t : TerminalSymbol, target) => (t, LRAction.Shift(target))}

      val result = (reduces ::: shifts).groupBy(_._1).toList.map { case (symbol, symbolActions) => (symbol, resolveConflictWithAttribute(symbol, symbolActions.map(_._2)))}

      val actionRow = actionTable(id)
      for ((t, as) <- result) actionRow(t.token.id) = as
    }

    new TableDrivenGLRParser(name, grammar, new CompressedLRActionTable(actionTable), new CompressedLRGotoTable(gotoTable))
  }
}

final class TableDrivenGLR1ParserBuilder(val grammar : Grammar) extends TableDrivenGLRParserBuilder {
  def name : String = TableDrivenGLR1ParserFactory.name
  def create : TableDrivenGLRParser = {
    val (transitions, id2Set) = buildCanonicalCollection(grammar.start.productions.map(LR1Item(_, 0, grammar.EOF)))
    create(transitions, id2Set, (id, item) => List(item.asInstanceOf[LR1Item].lookAhead))
  }
}

final class TableDrivenGLALRParserBuilder(val grammar : Grammar) extends TableDrivenGLRParserBuilder with ILALRParserBuilder[List[LRAction.Action]] {
  def name : String = TableDrivenGLALRParserFactory.name
  def create : TableDrivenGLRParser = {
    val (transitions, id2Set) = buildCanonicalCollection(grammar.start.productions.map(LR0Item(_, 0)))
    create(transitions, id2Set, generateLAFunc(transitions, id2Set))
  }
}

object TableDrivenGLR1ParserFactory extends IParserFactory {
  def ll : Boolean = false
  def name : String = "GLR1"
  def create(grammar : Grammar, reportConflict : Boolean) : IParser = new TableDrivenGLR1ParserBuilder(grammar).create
}

object TableDrivenGLALRParserFactory extends IParserFactory {
  def ll : Boolean = false
  def name : String = "GLALR"
  def create(grammar : Grammar, reportConflict : Boolean) : IParser = new TableDrivenGLALRParserBuilder(grammar).create
}
