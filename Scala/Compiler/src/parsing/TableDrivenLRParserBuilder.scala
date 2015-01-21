package parsing

import scala.collection.mutable

abstract class TableDrivenLRParserBuilder extends ILRParserBuilder[LRAction.Action] {
  protected def create(
    transitions : Array[List[(IGrammarSymbol, Int)]],
    id2Set : Array[List[LRItem]],
    item2LookAheads : (Int, LRItem) => List[TerminalSymbol],
    reportConflict : Boolean) : TableDrivenLRParser = {

    val actionTable = Array.fill(id2Set.size, grammar.maxTermID + 1)(null : LRAction.Action)
    val gotoTable = Array.fill(id2Set.size)(mutable.HashMap[String, Int]())
    var foundConflict = false

    for ((trans, id) <- transitions.zipWithIndex) {
      val gotoRow = gotoTable(id)
      for ((nt : INonTerminalSymbol, target) <- trans) gotoRow(nt.name) = target

      val reduces = for (item <- id2Set(id).filter(_.complete);
                         action = if (item.production.left == grammar.start) LRAction.Accept(item.production) else LRAction.Reduce(item.production);
                         term <- item2LookAheads(id, item)) yield (term, action)
      val shifts = trans.collect { case (t : TerminalSymbol, target) => (t, LRAction.Shift(target))}

      val result = (reduces ::: shifts).groupBy(_._1).toList.map { case (symbol, symbolActions) => (symbol, resolveConflictWithAttribute(symbol, symbolActions.map(_._2)))}

      val conflicts = result.filter(_._2.length > 1)
      if (reportConflict && conflicts.nonEmpty) {
        foundConflict = true
        println(s"Set_$id Found conflicts:")
        println(conflicts.map { case (symbol, actions) => s"\t$symbol\n\t\t${actions.mkString("\n\t\t")}"}.mkString("\n"))
      }

      val actionRow = actionTable(id)
      for ((t, as) <- result) actionRow(t.token.id) = as.last
    }

    if (foundConflict && id2Set.size < 512) {
      println("\nGrammar:\n")
      println(grammar)
      println("\nSetTable:\n")
      println(id2Set.zipWithIndex.map { case (set, id) => s"Set_$id:\n\t$set\n"}.mkString(""))
      println("\nTransitionTable:\n")
      println(transitions.zipWithIndex.map { case (trans, id) => s"Set_$id:\n\t$trans\n"}.mkString(""))
    }

    new TableDrivenLRParser(name, grammar, new CompressedLRActionTable(actionTable), new CompressedLRGotoTable(gotoTable))
  }
}

final class TableDrivenLR0ParserBuilder(val grammar : Grammar) extends TableDrivenLRParserBuilder {
  def name : String = TableDrivenLR0ParserFactory.name
  def create : TableDrivenLRParser = {
    val (transitions, id2Set) = buildCanonicalCollection(grammar.start.productions.map(LR0Item(_, 0)))
    create(transitions, id2Set, (id, item) => grammar.inputTerms, reportConflict = false)
  }
}

final class TableDrivenSLRParserBuilder(val grammar : Grammar, val reportConflict : Boolean) extends TableDrivenLRParserBuilder {
  def name : String = TableDrivenSLRParserFactory.name
  def create : TableDrivenLRParser = {
    val (transitions, id2Set) = buildCanonicalCollection(grammar.start.productions.map(LR0Item(_, 0)))
    create(transitions, id2Set, (id, item) => grammar.followMap(item.production.left).toList.map(grammar.id2Term), reportConflict)
  }
}

final class TableDrivenLR1ParserBuilder(val grammar : Grammar, val reportConflict : Boolean) extends TableDrivenLRParserBuilder {
  def name : String = TableDrivenLR1ParserFactory.name
  def create : TableDrivenLRParser = {
    val (transitions, id2Set) = buildCanonicalCollection(grammar.start.productions.map(LR1Item(_, 0, grammar.EOF)))
    create(transitions, id2Set, (id, item) => List(item.asInstanceOf[LR1Item].lookAhead), reportConflict)
  }
}

final class TableDrivenLALRParserBuilder(val grammar : Grammar, val reportConflict : Boolean) extends TableDrivenLRParserBuilder with ILALRParserBuilder[LRAction.Action] {
  def name : String = TableDrivenLALRParserFactory.name
  def create : TableDrivenLRParser = {
    val (transitions, id2Set) = buildCanonicalCollection(grammar.start.productions.map(LR0Item(_, 0)))
    create(transitions, id2Set, generateLAFunc(transitions, id2Set), reportConflict)
  }
}

object TableDrivenLR0ParserFactory extends IParserFactory {
  def ll : Boolean = false
  def name : String = "LR0"
  def create(grammar : Grammar, reportConflict : Boolean) = new TableDrivenLR0ParserBuilder(grammar).create
}

object TableDrivenSLRParserFactory extends IParserFactory {
  def ll : Boolean = false
  def name : String = "SLR"
  def create(grammar : Grammar, reportConflict : Boolean) = new TableDrivenSLRParserBuilder(grammar, reportConflict).create
}

object TableDrivenLR1ParserFactory extends IParserFactory {
  def ll : Boolean = false
  def name : String = "LR1"
  def create(grammar : Grammar, reportConflict : Boolean) : IParser = new TableDrivenLR1ParserBuilder(grammar, reportConflict).create
}

object TableDrivenLALRParserFactory extends IParserFactory {
  def ll : Boolean = false
  def name : String = "LALR"
  def create(grammar : Grammar, reportConflict : Boolean) : IParser = new TableDrivenLALRParserBuilder(grammar, reportConflict).create
}
