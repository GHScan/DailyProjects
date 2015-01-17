package parsing

import scala.collection.{immutable, mutable}

abstract class TableDrivenLRParserBuilder {
  def name : String
  def create : LRParser
  def grammar : Grammar

  protected trait LRItem {
    def production : IProduction
    def pos : Int
    def closure() : List[LRItem]
    def move() : Option[(IGrammarSymbol, LRItem)]
    def complete : Boolean = pos == production.right.length
  }

  protected case class LR0Item(production : IProduction, pos : Int) extends LRItem {
    def closure() : List[LR0Item] = production.right.drop(pos) match {
      case (nt : INonTerminalSymbol) :: tail => nt.productions.map(LR0Item(_, 0))
      case _ => Nil
    }
    def move() : Option[(IGrammarSymbol, LR0Item)] = production.right.drop(pos) match {
      case head :: _ => Some((head, LR0Item(production, pos + 1)))
      case _ => None
    }
    override def toString = new Production(production.left, production.right.take(pos) ::: new NonTerminalSymbol("·", Nil) :: production.right.drop(pos), null).toString
  }

  protected case class LR1Item(production : IProduction, pos : Int, lookAhead : TerminalSymbol) extends LRItem {
    def closure() : List[LR1Item] = production.right.drop(pos) match {
      case (nt : INonTerminalSymbol) :: tail =>
        for (la <- grammar.firstOfSymbols(tail ::: List(lookAhead), grammar.firstMap).toList.map(grammar.id2Term);
             p <- nt.productions) yield LR1Item(p, 0, la)
      case _ => Nil
    }
    def move() : Option[(IGrammarSymbol, LR1Item)] = production.right.drop(pos) match {
      case head :: _ => Some((head, LR1Item(production, pos + 1, lookAhead)))
      case _ => None
    }
  }

  protected def closure(set : List[LRItem]) : List[LRItem] = {
    val c = mutable.Set[LRItem](set : _*)

    var workList = set
    while (workList.nonEmpty) {
      val head = workList.head
      workList = workList.tail

      for (item <- head.closure() if !c(item)) {
        workList = item :: workList
        c += item
      }
    }

    c.toList
  }

  protected def buildCanonicalCollection(start : List[LRItem]) : (Array[List[(IGrammarSymbol, Int)]], Array[List[LRItem]]) = {
    val set2ID = mutable.Map[List[LRItem], Int](closure(start) -> 0)
    val transitions = mutable.ArrayBuffer[List[(IGrammarSymbol, Int)]](Nil)

    var workList = List(set2ID.head._1)
    while (workList.nonEmpty) {
      val set = workList.head
      val id = set2ID(set)
      workList = workList.tail

      transitions(id) =
        (for (Some((symbol, targetItem)) <- set.map(_.move())) yield (symbol, targetItem)).groupBy(_._1).toList
          .map { case (symbol, symbolItems) =>
          val targetSet = closure(symbolItems.map(_._2))
          (symbol, set2ID.getOrElseUpdate(targetSet, {
            transitions += Nil
            workList = targetSet :: workList
            set2ID.size
          }))
        }
    }

    (transitions.toArray, set2ID.toList.sortBy(_._2).map(_._1).toArray)
  }

  private def resolveConflictWithAttribute(symbol : TerminalSymbol, actions : List[LRAction.Action]) : List[LRAction.Action] = {
    val attributeActions = actions.map {
      case a@LRAction.Accept(p) => (grammar.attributeOfProduction(p), a)
      case a@LRAction.Reduce(p) => (grammar.attributeOfProduction(p), a)
      case a@LRAction.Shift(target) => (grammar.attributeOfSymbol(symbol), a)
    }
    if (attributeActions.exists(_._1 == None)) return actions

    val maxPriority = attributeActions.iterator.map(_._1.get.priority).max
    attributeActions.filter(_._1.get.priority == maxPriority) match {
      case List((_, a)) => List(a)
      case l@((attr, _) :: _) if attr.get.associativity == Associativity.Left =>
        l.collect {
          case ((_, a : LRAction.Accept)) => a
          case ((_, a : LRAction.Reduce)) => a
        }
      case l =>
        l.collect {
          case ((_, a : LRAction.Shift)) => a
        }
    }
  }

  protected def create(
    transitions : Array[List[(IGrammarSymbol, Int)]],
    id2Set : Array[List[LRItem]],
    item2LookAheads : (Int, LRItem) => List[TerminalSymbol],
    reportConflict : Boolean = true) : TableDrivenLRParser = {

    val actionTable = Array.fill(id2Set.size, grammar.maxTermID + 1)(null : LRAction.Action)
    val gotoTable = Array.fill(id2Set.size)(mutable.HashMap[String, Int]())
    var foundConflict = false

    for ((trans, id) <- transitions.zipWithIndex) {
      val gotoRow = gotoTable(id)
      for ((nt : INonTerminalSymbol, target) <- trans) {
        gotoRow(nt.name) = target
      }

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

    new TableDrivenLRParser(name, new CompressedLRActionTable(actionTable), new CompressedLRGotoTable(gotoTable))
  }
}

final class TableDrivenLR0ParserBuilder(val grammar : Grammar, val reportConflict : Boolean) extends TableDrivenLRParserBuilder {
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

final class TableDrivenLALRParserBuilder(val grammar : Grammar, val reportConflict : Boolean) extends TableDrivenLRParserBuilder {
  def name : String = TableDrivenLALRParserFactory.name
  def create : TableDrivenLRParser = {
    import immutable.BitSet
    type LALRItem = (Int, LRItem)

    val (transitions, id2Set) = buildCanonicalCollection(grammar.start.productions.map(LR0Item(_, 0)))
    val itemSpreadMap = mutable.Map[LALRItem, List[LALRItem]]()
    val item2LAs = mutable.Map[LALRItem, BitSet]()

    closure(grammar.start.productions.map(LR1Item(_, 0, grammar.EOF))).foreach { case item : LR1Item =>
      val lalrItem = (0, LR0Item(item.production, item.pos))
      item2LAs(lalrItem) = grammar.firstMap(item.lookAhead) | item2LAs.getOrElse(lalrItem, BitSet.empty)
    }

    for (id <- 0 until transitions.length;
         item <- id2Set(id);
         moveResult = LR1Item(item.production, item.pos, grammar.ERROR).move() if moveResult.nonEmpty;
         targetID = transitions(id).find(_._1 == moveResult.get._1).get._2;
         targetLR1Item <- closure(List(moveResult.get._2)).iterator.map(_.asInstanceOf[LR1Item])) {
      val srcLALRItem = (id, item)
      val targetLALRItem = (targetID, LR0Item(targetLR1Item.production, targetLR1Item.pos))
      if (targetLR1Item.lookAhead == grammar.ERROR) {
        itemSpreadMap(srcLALRItem) = targetLALRItem :: itemSpreadMap.getOrElse(srcLALRItem, Nil)
      } else {
        item2LAs(targetLALRItem) = item2LAs.getOrElse(targetLALRItem, BitSet.empty) + targetLR1Item.lookAhead.token.id
      }
    }

    var changed = true
    while (changed) {
      changed = false

      for ((item, las) <- item2LAs;
           target <- itemSpreadMap.getOrElse(item, Nil)) {
        val oldLAs = item2LAs.getOrElse(target, BitSet.empty)
        val newLAs = oldLAs | las
        if (oldLAs != newLAs) {
          changed = true
          item2LAs(target) = newLAs
        }
      }
    }

    create(transitions, id2Set, (id, item) => item2LAs.getOrElse((id, item), BitSet.empty).toList.map(grammar.id2Term), reportConflict)
  }
}

object TableDrivenLR0ParserFactory extends IParserFactory {
  def ll : Boolean = false
  def name : String = "LR0"
  def create(grammar : Grammar, reportConflict : Boolean) = new TableDrivenLR0ParserBuilder(grammar, reportConflict).create
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
