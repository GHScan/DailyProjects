import scala.collection.immutable.BitSet
import scala.collection.{immutable, mutable}

object Test extends App {

  //----------------------------------------------------------------------------
  abstract class GrammarSymbol {
    def ~(other : GrammarSymbol) = new Concat(this, other)

    def |(other : GrammarSymbol) = new Or(this, other)
  }

  case class Term(name : String) extends GrammarSymbol {
    override def toString = s"'$name'"
  }

  case class NonTerm(name : String) extends GrammarSymbol with Ordered[NonTerm] {
    override def compare(that : NonTerm) : Int = name.compare(that.name)

    override def toString = name
  }

  object Empty extends Term("ε") {
    override def toString = "ε"
  }

  object Eof extends Term("EOF") {
    override def toString = "EOF"
  }

  object SPECIAL_TERM extends Term("SPECIAL_" + new {}.hashCode().toString()) {
    override def toString = sys.error("SPECIAL_TERM")
  }

  case class Concat(first : GrammarSymbol, second : GrammarSymbol) extends GrammarSymbol {
    override def toString = sys.error("Concat")
  }

  case class Or(first : GrammarSymbol, second : GrammarSymbol) extends GrammarSymbol {
    override def toString = sys.error("Or")
  }

  //----------------------------------------------------------------------------
  case class Production(nonTerm : NonTerm, body : List[GrammarSymbol]) {
    override def toString = s"$nonTerm => ${body.mkString(" ")}"

    def bodyDrop(len : Int) = body.drop(len) match {
      case Nil => List(Empty)
      case l => l
    }
  }

  class Grammar(startSymbol : NonTerm, productions : List[Production] = Nil) {
    def rule(nonTerm : NonTerm)(body : GrammarSymbol) = {
      def flat(symbol : GrammarSymbol) : List[List[GrammarSymbol]] = symbol match {
        case Or(first, second) => flat(first) ++ flat(second)
        case Concat(first, second) => for (i <- flat(first); j <- flat(second)) yield i ::: j
        case _ => List(List(symbol))
      }
      new Grammar(startSymbol, flat(body).map(Production(nonTerm, _)) ::: productions)
    }

    lazy val nonTerms : List[NonTerm] = {
      productions.flatMap(p => p.nonTerm :: p.body.collect { case t : NonTerm => t}).distinct.sorted
    }

    lazy val nonTerm2Productions = productions.groupBy(_.nonTerm)

    lazy val terms : List[Term] = {
      (Eof :: Empty :: SPECIAL_TERM :: productions.flatMap(p => p.body.collect { case t : Term => t})).distinct
    }

    lazy val term2ID = terms.zipWithIndex.toMap
    lazy val id2Term = terms.toArray
    lazy val emptyID = term2ID(Empty)

    override def toString = s"start: $startSymbol\n" + productions.map(_.toString).mkString("\n") + "\n"

    lazy val firstSet : mutable.Map[GrammarSymbol, BitSet] = {
      val set = mutable.Map[GrammarSymbol, BitSet](terms.map(t => (t, BitSet(term2ID(t)))) : _*)

      def firstOfSymbols(symbols : Seq[GrammarSymbol]) : BitSet = symbols match {
        case Nil => set(Empty)
        case head :: tail =>
          val first = set.getOrElse(head, BitSet.empty)
          if (first.contains(emptyID)) (first - emptyID) | firstOfSymbols(tail) else first
      }

      var changed = true
      while (changed) {
        changed = false
        for ((nt, ps) <- nonTerm2Productions) {
          val first = ps.foldLeft(BitSet.empty) { (s, p) => s | firstOfSymbols(p.body)}
          val oldFirst = set.getOrElse(nt, BitSet.empty)
          if (first != oldFirst) changed = true
          set(nt) = first
        }
      }

      set
    }

    def firstOfSymbols(symbols : Seq[GrammarSymbol]) : BitSet = symbols match {
      case Nil => firstSet(Empty)
      case head :: tail =>
        val first = firstSet(head)
        if (first.contains(emptyID)) (first - emptyID) | firstOfSymbols(tail) else first
    }


    lazy val followSet : mutable.Map[NonTerm, BitSet] = {
      val set = mutable.Map[NonTerm, BitSet](startSymbol -> firstSet(Eof))

      var changed = true
      while (changed) {
        changed = false
        for (p <- productions) {
          var follow = set.getOrElse(p.nonTerm, BitSet.empty)
          p.body.reverse.foreach {
            case t : Term => follow = firstSet(t)
            case nt : NonTerm =>
              val oldFollow = set.getOrElse(nt, BitSet.empty)
              val newFollow = oldFollow | follow
              if (oldFollow != newFollow) changed = true
              set(nt) = newFollow
              val first = firstSet(nt)
              follow = if (first.contains(emptyID)) follow | first else first
          }
        }
      }

      set
    }

    trait LRItem {
      def closure() : List[LRItem]

      def move(symbol : GrammarSymbol) : Option[LRItem]
    }

    case class LR0Item(production : Production, pos : List[GrammarSymbol]) extends LRItem {
      def closure() : List[LR0Item] = pos match {
        case (nt : NonTerm) :: tail => nonTerm2Productions(nt).map(p => LR0Item(p, p.body))
        case _ => Nil
      }

      def move(symbol : GrammarSymbol) : Option[LR0Item] = pos match {
        case head :: tail if head == symbol => Some(LR0Item(production, pos.tail))
        case _ => None
      }

      override def toString = Production(production.nonTerm, production.body.take(production.body.length - pos.length) ::: NonTerm("·") :: pos).toString
    }

    case class LR1Item(production : Production, pos : List[GrammarSymbol], lookAhead : Term) extends LRItem {
      def closure() : List[LR1Item] = pos match {
        case (nt : NonTerm) :: tail =>
          (for (tid <- firstOfSymbols(tail ::: List(lookAhead));
                p <- nonTerm2Productions(nt)) yield LR1Item(p, p.body, id2Term(tid))).toList
        case _ => Nil
      }

      def move(symbol : GrammarSymbol) : Option[LR1Item] = pos match {
        case head :: tail if head == symbol => Some(LR1Item(production, pos.tail, lookAhead))
        case _ => None
      }

      override def toString = s"${Production(production.nonTerm, production.body.take(production.body.length - pos.length) ::: NonTerm("·") :: pos)}， $lookAhead"
    }

    def closure(itemSet : immutable.Set[LRItem]) : immutable.Set[LRItem] = {
      var set = itemSet

      var workList = itemSet.toList
      while (workList.nonEmpty) {
        val item = workList.head
        workList = workList.tail
        for (i <- item.closure()) {
          if (!set(i)) {
            set += i
            workList = i :: workList
          }
        }
      }

      set
    }

    def move(itemSet : immutable.Set[LRItem], symbol : GrammarSymbol) : immutable.Set[LRItem] = {
      itemSet.iterator.map(_.move(symbol)).collect { case Some(i) => i}.toSet
    }

    def generateCanonicalCollection(itemSet : immutable.Set[LRItem]) : (Array[List[(GrammarSymbol, Int)]], Array[immutable.Set[LRItem]]) = {
      val dfa = mutable.ArrayBuffer[List[(GrammarSymbol, Int)]](Nil)
      val set2ID = mutable.Map[immutable.Set[LRItem], Int](itemSet -> 0)

      var workList = List(itemSet)
      while (workList.nonEmpty) {
        val set = workList.head
        workList = workList.tail

        val id = set2ID(set)
        val setClosure = closure(set)
        dfa(id) = (
          for (symbol <- terms.iterator ++ nonTerms.iterator;
               newState = move(setClosure, symbol) if newState.nonEmpty) yield
            (symbol, set2ID.getOrElseUpdate(newState, {
              workList = newState :: workList
              dfa += Nil
              set2ID.size
            }))).toList
      }

      (dfa.toArray, set2ID.toList.sortBy(_._2).map(_._1).toArray)
    }

    def generateLR0Machine(reportConflict : Boolean = true) : LRMachine = {
      val (dfa, id2Set) = generateCanonicalCollection(immutable.Set(nonTerm2Productions(startSymbol).map(p => LR0Item(p, p.body)) : _*))
      new LRMachine(dfa.zipWithIndex.map { case (l, id) =>
        val gotoMap : Map[GrammarSymbol, LRAction] = l.collect { case ((nt : NonTerm, target)) => (nt, Goto(target))}.toMap
        val shiftMap : Map[GrammarSymbol, LRAction] = l.collect { case ((t : Term, target)) => (t, Shift(target))}.toMap
        val reduces = id2Set(id).toList.map(_.asInstanceOf[LR0Item]).filter(_.pos.isEmpty)

        if (reportConflict) {
          if (reduces.length > 1) println(s"Found LR0 reduce-reduce conflict: \n$reduces \n")
          if (reduces.nonEmpty && shiftMap.nonEmpty) println(s"Found LR0 shift-reduce conflict: \n$shiftMap \n$reduces \n")
        }

        val reduceMap : Map[GrammarSymbol, LRAction] = if (reduces.nonEmpty) {
          val reduce = reduces.find(_.production.nonTerm == startSymbol) match {
            case None => Reduce(reduces.head.production)
            case Some(item) => Accept
          }
          terms.diff(shiftMap.keys.toList).map((_, reduce)).toMap
        } else Map.empty

        gotoMap ++ shiftMap ++ reduceMap
      })
    }

    def generateSLRMachine(reportConflict : Boolean = true) : LRMachine = {
      val (dfa, id2Set) = generateCanonicalCollection(immutable.Set(nonTerm2Productions(startSymbol).map(p => LR0Item(p, p.body)) : _*))
      new LRMachine(dfa.zipWithIndex.map { case (l, id) =>
        val gotoMap : Map[GrammarSymbol, LRAction] = l.collect { case ((nt : NonTerm, target)) => (nt, Goto(target))}.toMap
        val shiftMap : Map[GrammarSymbol, LRAction] = l.collect { case ((t : Term, target)) => (t, Shift(target))}.toMap
        val reduceMaps = id2Set(id).toList.map(_.asInstanceOf[LR0Item]).filter(_.pos.isEmpty).map { item =>
          val action = if (item.production.nonTerm == startSymbol) Accept else Reduce(item.production)
          followSet(item.production.nonTerm).iterator.map(id2Term).map((_ : GrammarSymbol, action)).toMap
        }

        if (reportConflict) {
          val shiftTerms = shiftMap.keys.toSet
          for (rm <- reduceMaps if rm.keys.exists(shiftTerms)) println(s"Found SLR shift-reduce conflict: \n$shiftMap\n$rm \n")
          if (reduceMaps.map(_.size).sum > reduceMaps.flatMap(_.keys).distinct.length) {
            println(s"Found SLR reduce-reduce conflict: \n$reduceMaps \n")
          }
        }

        gotoMap ++ shiftMap ++ reduceMaps.fold(Map.empty)(_ ++ _)
      })
    }

    def generateLR1Machine(reportConflict : Boolean = true) : LRMachine = {
      val (dfa, id2Set) = generateCanonicalCollection(immutable.Set(nonTerm2Productions(startSymbol).map(p => LR1Item(p, p.body, Eof)) : _*))
      new LRMachine(dfa.zipWithIndex.map { case (l, id) =>
        val gotoMap : Map[GrammarSymbol, LRAction] = l.collect { case ((nt : NonTerm, target)) => (nt, Goto(target))}.toMap
        val shiftMap : Map[GrammarSymbol, LRAction] = l.collect { case ((t : Term, target)) => (t, Shift(target))}.toMap
        val reduces = id2Set(id).toList.map(_.asInstanceOf[LR1Item]).filter(_.pos.isEmpty).map { item =>
          (item.lookAhead : GrammarSymbol, if (item.production.nonTerm == startSymbol) Accept else Reduce(item.production) : LRAction)
        }
        val reduceMap = reduces.toMap

        if (reportConflict) {
          if (reduceMap.size < reduces.length) println(s"Found LR1 reduce-reduce conflict: \n$reduces \n")
          if ((reduceMap.keySet & shiftMap.keySet).nonEmpty) println(s"Found LR1 shift-reduce conflict: \n$shiftMap\n$reduceMap \n\n")
        }

        gotoMap ++ shiftMap ++ reduceMap
      })
    }

    def generateLALRMachine(reportConflict : Boolean = true) : LRMachine = {
      type LALRItem = (Int, LRItem)
      val (dfa, id2Set) = generateCanonicalCollection(immutable.Set(nonTerm2Productions(startSymbol).map(p => LR0Item(p, p.body)) : _*))
      val item2LAs = mutable.Map[LALRItem, BitSet](id2Set(0).toList.map(item => ((0, item), firstSet(Eof))) : _*)
      val itemSpreadMap = mutable.Map[LALRItem, List[LALRItem]]()

      for ((set, id) <- id2Set.zipWithIndex;
           item <- set.asInstanceOf[Iterable[LR0Item]];
           srcLALRItem = (id, item : LRItem);
           itemClosure = closure(immutable.Set(LR1Item(item.production, item.pos, SPECIAL_TERM)));
           symbol <- terms.iterator ++ nonTerms.iterator;
           targetState = move(itemClosure, symbol) if targetState.nonEmpty;
           targetID = dfa(id).find(_._1 == symbol).get._2;
           targetItem <- targetState.asInstanceOf[Iterable[LR1Item]];
           targetLALRItem = (targetID, LR0Item(targetItem.production, targetItem.pos))) {
        targetItem.lookAhead match {
          case SPECIAL_TERM => itemSpreadMap(srcLALRItem) = targetLALRItem :: itemSpreadMap.getOrElse(srcLALRItem, Nil)
          case t => item2LAs(targetLALRItem) = item2LAs.getOrElse(targetLALRItem, BitSet.empty) + term2ID(t)
        }
      }

      var changed = true
      while (changed) {
        changed = false
        for ((item, las) <- item2LAs;
             target <- itemSpreadMap.getOrElse(item, Nil);
             oldLas = item2LAs.getOrElse(target, BitSet.empty);
             newLas = oldLas | las if oldLas != newLas) {
          changed = true
          item2LAs(target) = newLas
        }
      }

      new LRMachine(dfa.zipWithIndex.map { case (l, id) =>
        val gotoMap : Map[GrammarSymbol, LRAction] = l.collect { case ((nt : NonTerm, target)) => (nt, Goto(target))}.toMap
        val shiftMap : Map[GrammarSymbol, LRAction] = l.collect { case ((t : Term, target)) => (t, Shift(target))}.toMap
        val reduceMaps = id2Set(id).toList.map(_.asInstanceOf[LR0Item]).filter(_.pos.isEmpty).map { item =>
          val action = if (item.production.nonTerm == startSymbol) Accept else Reduce(item.production)
          item2LAs((id, item : LRItem)).map(id2Term).map((_ : GrammarSymbol, action)).toMap
        }

        if (reportConflict) {
          val shiftTerms = shiftMap.keys.toSet
          for (rm <- reduceMaps if rm.keys.exists(shiftTerms)) println(s"Found LALR shift-reduce conflict: \n$shiftMap\n$rm \n")
          if (reduceMaps.map(_.size).sum > reduceMaps.flatMap(_.keys).distinct.length) {
            println(s"Found LALR reduce-reduce conflict: \n$reduceMaps \n")
          }
        }

        gotoMap ++ shiftMap ++ reduceMaps.fold(Map.empty)(_ ++ _)
      })
    }

  }

  abstract class LRAction

  case class Shift(state : Int) extends LRAction

  case class Reduce(production : Production) extends LRAction

  case object Accept extends LRAction

  case class Goto(state : Int) extends LRAction

  class LRMachine(transitions : Array[Map[GrammarSymbol, LRAction]]) {
    override def toString = s"states:${transitions.length} \n${transitions.zipWithIndex.map { case (t, i) => s"$i: $t"}.mkString("\n")}"

    def stateCount = transitions.length

    def parse(_source : Seq[Term]) : String = {
      var stack = immutable.Stack[Int]()
      stack = stack.push(0)

      try {
        var source = _source ++ List(Eof)
        while (stack.nonEmpty && source.nonEmpty) {
          transitions(stack.top)(source.head) match {
            case Shift(state) =>
              source = source.tail
              stack = stack.push(state)
            case Reduce(production) => {
              stack = stack.drop(production.body.length)
              transitions(stack.top)(production.nonTerm) match {
                case Goto(state) => stack = stack.push(state)
              }
            }
            case Accept if source == List(Eof) => return "Success"
          }
        }
      } catch {
        case e : Exception => return "Failed"
      }

      "Failed"
    }
  }

  //----------------------------------------------------------------------------
  implicit def str2Term(s : String) : Term = Term(s)

  implicit def symbol2NonTerm(s : Symbol) : NonTerm = NonTerm(s.toString().substring(1))

  def buildGrammar() : Grammar = {
    new Grammar('program)
      .rule('program)(
        'exp
      )
      .rule('exp)(
        'exp ~ ("+" | "-") ~ 'term
          | 'term
      )
      .rule('exp_list)(
        'exp
          | 'exp_list ~ "," ~ 'exp
      )
      .rule('opt_exp_list)(
        Empty
          | 'exp_list
      )
      .rule('term)(
        'term ~ ("*" | "/") ~ 'factor
          | 'factor
      )
      .rule('factor)(
        "NUMBER"
          | "IDENT"
          | "IDENT" ~ "[" ~ 'opt_exp_list ~ "]"
          | "IDENT" ~ "(" ~ 'opt_exp_list ~ ")"
          | "(" ~ 'exp ~ ")"
      )
  }

  //----------------------------------------------------------------------------
  val input = List[Term](
    "IDENT", "+", "(", "IDENT", "[", "NUMBER", "]", "-", "NUMBER", "*", "IDENT", ")"
  )
  val illegalInput = List[Term](
    "IDENT", "+", "(", "IDENT", "[", "NUMBER", ")", "-", "NUMBER", "*", "IDENT", ")"
  )
  val short1 = List[Term]("IDENT", "+", "(", "NUMBER", "-", "IDENT", ")")
  val short2 = List[Term]("IDENT", "+", "NUMBER", "-", "IDENT", ")")
  val profileInput = List[Term](
    "IDENT", "+", "(", "IDENT", "[", "NUMBER", "]", "-", "NUMBER", "*", "IDENT", ")", "*", "NUMBER", "/", "IDENT", "+", "NUMBER", "*", "IDENT", "(", "IDENT", ")", "+", "(", "IDENT", "[", "NUMBER", "]", "-", "NUMBER", "*", "IDENT", ")", "*", "NUMBER", "/", "IDENT", "+", "NUMBER", "*", "IDENT", "(", "IDENT", ")"
  )

  val g = buildGrammar()

  val lr0 = g.generateLR0Machine()
  println("states:", lr0.stateCount)
  println(lr0.parse(input))
  println(lr0.parse(illegalInput))
  println(lr0.parse(profileInput))
  println(lr0.parse(short1))
  println(lr0.parse(short2))

  val slr = g.generateSLRMachine()
  println("states:", slr.stateCount)
  println(slr.parse(input))
  println(slr.parse(illegalInput))
  println(slr.parse(profileInput))
  println(slr.parse(short1))
  println(slr.parse(short2))

  val lr1 = g.generateLR1Machine()
  println("states:", lr1.stateCount)
  println(lr1.parse(input))
  println(lr1.parse(illegalInput))
  println(lr1.parse(profileInput))
  println(lr1.parse(short1))
  println(lr1.parse(short2))

  val lalr = g.generateLALRMachine()
  println("states:", lalr.stateCount)
  println(lalr.parse(input))
  println(lalr.parse(illegalInput))
  println(lalr.parse(profileInput))
  println(lalr.parse(short1))
  println(lalr.parse(short2))

  val kBuildTimes = 20
  val kMeasureTimes = 5
  Utils.timeit(s"Build LR0 $kBuildTimes times", kMeasureTimes) {
    for (_ <- 0 until kBuildTimes) g.generateLR0Machine(reportConflict = false)
  }
  Utils.timeit(s"Build SLR $kBuildTimes times", kMeasureTimes) {
    for (_ <- 0 until kBuildTimes) g.generateSLRMachine(reportConflict = false)
  }
  Utils.timeit(s"Build LR1 $kBuildTimes times", kMeasureTimes) {
    for (_ <- 0 until kBuildTimes) g.generateLR1Machine(reportConflict = false)
  }
  Utils.timeit(s"Build LALR $kBuildTimes times", kMeasureTimes) {
    for (_ <- 0 until kBuildTimes) g.generateLALRMachine(reportConflict = false)
  }

  val kParseTimes = 3000
  val kMeasureTimes2 = 5
  Utils.timeit(s"Parse $kParseTimes with LR0", kMeasureTimes2) {
    for (_ <- 0 until kParseTimes) lr0.parse(profileInput)
  }
  Utils.timeit(s"Parse $kParseTimes with SLR", kMeasureTimes2) {
    for (_ <- 0 until kParseTimes) slr.parse(profileInput)
  }
  Utils.timeit(s"Parse $kParseTimes with LR1", kMeasureTimes2) {
    for (_ <- 0 until kParseTimes) lr1.parse(profileInput)
  }
  Utils.timeit(s"Parse $kParseTimes with LALR", kMeasureTimes2) {
    for (_ <- 0 until kParseTimes) lalr.parse(profileInput)
  }
}
