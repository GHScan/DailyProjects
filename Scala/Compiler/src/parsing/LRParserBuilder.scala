package parsing

import scala.collection.mutable

trait ILRParserBuilder[ActionT] {
  def name : String
  def create : LRParser[ActionT]
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

  protected def resolveConflictWithAttribute(symbol : TerminalSymbol, actions : List[LRAction.Action]) : List[LRAction.Action] = {
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
}


trait ILALRParserBuilder[ActionT] extends ILRParserBuilder[ActionT] {
  def generateLAFunc(transitions : Array[List[(IGrammarSymbol, Int)]], id2Set : Array[List[LRItem]]) : (Int, LRItem) => List[TerminalSymbol] = {
    import scala.collection.immutable.BitSet
    type LALRItem = (Int, LRItem)

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

    (id, item) => item2LAs.getOrElse((id, item), BitSet.empty).toList.map(grammar.id2Term)
  }
}
