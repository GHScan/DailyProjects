package parsing

import scala.collection.immutable.BitSet
import scala.collection.{immutable, mutable}

class Grammar(
  val start : INonTerminalSymbol,
  val terminalSymbol2Attribute : immutable.Map[TerminalSymbol, TerminalSymbolAttribute],
  val syncWord2ErrorRecoveryAction : Map[lexical.IToken, ErrorRecoveryAction]) {

  override def toString = s"start=$start\n\t${productions.mkString("\n\t")}\n"

  def attributeOfSymbol(symbol : IGrammarSymbol) : Option[TerminalSymbolAttribute] = symbol match {
    case t : TerminalSymbol => terminalSymbol2Attribute.get(t)
    case _ => None
  }
  def attributeOfProduction(p : IProduction) : Option[TerminalSymbolAttribute] = p.right.collectFirst {
    case t : TerminalSymbol if terminalSymbol2Attribute.contains(t) => terminalSymbol2Attribute(t)
  }

  val EMPTY = TerminalSymbol.EMPTY
  val EOF = TerminalSymbol.EOF
  val ERROR = TerminalSymbol.ERROR

  val nonTerms : List[INonTerminalSymbol] = {
    var set = Set[INonTerminalSymbol](start)
    var workList = List(start)

    while (workList.nonEmpty) {
      val head = workList.head
      workList = workList.tail
      for (p <- head.productions;
           symbol <- p.right.collect { case nt : INonTerminalSymbol => nt} if !set(symbol)) {
        workList = symbol :: workList
        set += symbol
      }
    }

    set.toList.sorted
  }

  val productions : List[IProduction] = nonTerms.flatMap(_.productions)

  val terms : List[TerminalSymbol] = List(EMPTY, ERROR, EOF) ::: productions.flatMap(_.right.collect { case t : TerminalSymbol => t}).distinct.sorted

  val maxTermID = terms.maxBy(_.token.id).token.id

  val inputTerms : List[TerminalSymbol] = terms.filter(t => t != EMPTY && t != ERROR)

  val id2Term : Array[TerminalSymbol] = {
    val a = Array.fill(maxTermID + 1)(terms.head)
    terms.foreach { t => a(t.token.id) = t}
    a
  }

  private def mutableNonTerms() : List[NonTerminalSymbol] = {
    val name2NewNonTerm = nonTerms.map(nt => (nt.name, new NonTerminalSymbol(nt.name, Nil))).toMap
    for (nt <- nonTerms;
         nnt = name2NewNonTerm(nt.name)) {
      nnt.productions = nt.productions.map(p => new Production(
        name2NewNonTerm(p.left.name),
        p.right.map {
          case nt : INonTerminalSymbol => name2NewNonTerm(nt.name)
          case o => o
        },
        p.action))
    }
    name2NewNonTerm.values.toList.sorted[INonTerminalSymbol]
  }

  def removeLeftRecursion() : Grammar = {
    val newNonTerms = mutableNonTerms()

    for (i <- newNonTerms) {
      var ps = i.productions
      for (j <- newNonTerms.takeWhile(_ < i)) {
        ps = ps.flatMap {
          p => p.right match {
            case head :: tail if head == j => j.productions.map(pj => new Production(i, pj.right ::: tail, { stack =>
              val (values, stack2) = stack.splitAt(tail.length)
              val stack3 = pj.action(stack2)
              p.action(values ::: stack3)
            }))
            case _ => List(p)
          }
        }
      }

      var id = 0
      def iterate() : Unit = {
        ps.find(_.rightHead == i) match {
          case Some(pl) =>
            val nnt = new NonTerminalSymbol(s"${i.name}_rr$id", Nil)
            nnt.productions = List(
              new Production(nnt, pl.right.tail ::: List(nnt), { stack =>
                val ll = stack.head.asInstanceOf[List[List[Any]]]
                val (values, stack2) = stack.tail.splitAt(pl.right.tail.length)
                (values :: ll) :: stack2
              }),
              new Production(nnt, Nil, { stack =>
                Nil :: stack
              }))

            ps = ps.filter(_ != pl).map(p => new Production(i, p.right ::: List(nnt), { stack =>
              val ll = stack.head.asInstanceOf[List[List[Any]]]
              var stack2 = p.action(stack.tail)
              for (l <- ll) stack2 = pl.action(l ::: stack2)
              stack2
            }))

            id += 1
            iterate()
          case None =>
        }
      }
      iterate()

      i.productions = ps
    }

    new Grammar(newNonTerms.find(_.name == start.name).get, terminalSymbol2Attribute, syncWord2ErrorRecoveryAction)
  }

  def leftFactoring() : Grammar = {
    val newNonTerms = mutableNonTerms()

    def iterate(nt : NonTerminalSymbol) : Unit = {
      var id = 0
      nt.productions = nt.productions.groupBy(_.rightHead).toList.map {
        case (_, List(p)) => p
        case (_, ps) =>
          var len = 1
          while (len < ps.head.right.length && ps.forall(p => len < p.right.length && p.right(len) == ps.head.right(len))) {
            len += 1
          }

          val nnt = new NonTerminalSymbol(s"${nt.name}_lf$id", Nil)
          nnt.productions = ps.map { p => new Production(nnt, p.right.drop(len), stack => p.action :: stack)}

          iterate(nnt)

          id += 1
          new Production(nt, ps.head.right.take(len) ::: List(nnt), { stack =>
            val action = stack.head.asInstanceOf[List[Any] => List[Any]]
            action(stack.tail)
          })
      }
    }
    newNonTerms.foreach(iterate)

    new Grammar(newNonTerms.find(_.name == start.name).get, terminalSymbol2Attribute, syncWord2ErrorRecoveryAction)
  }

  def firstOfSymbols(symbols : List[IGrammarSymbol], map : mutable.Map[IGrammarSymbol, BitSet]) : BitSet = symbols match {
    case Nil => map(EMPTY)
    case head :: tail =>
      val first = map(head)
      if (first.contains(EMPTY.token.id)) (first - EMPTY.token.id) | firstOfSymbols(tail, map) else first
  }

  lazy val firstMap : mutable.Map[IGrammarSymbol, BitSet] = {
    val m = mutable.Map(terms.map(t => (t : IGrammarSymbol, BitSet(t.token.id))) : _*)
    nonTerms.foreach { nt => m(nt) = BitSet.empty}

    var changed = true
    while (changed) {
      changed = false

      for (nt <- nonTerms) {
        val newFirst = nt.productions.foldLeft(BitSet.empty) { case (s, p) => s | firstOfSymbols(p.right, m)}
        val oldFirst = m(nt)
        if (newFirst != oldFirst) {
          m(nt) = newFirst
          changed = true
        }
      }
    }

    m
  }

  lazy val followMap : mutable.Map[INonTerminalSymbol, BitSet] = {
    val m = mutable.Map(nonTerms.map(nt => (nt, BitSet.empty)) : _*)
    m(start) = firstMap(EOF)

    var changed = true
    while (changed) {
      changed = false

      for (p <- productions) {
        var follow = m(p.left)
        p.right.reverseIterator.foreach {
          case nt : INonTerminalSymbol =>
            val oldFollow = m(nt)
            val newFollow = oldFollow | follow
            if (oldFollow != newFollow) {
              m(nt) = newFollow
              changed = true
            }
            val first = firstMap(nt)
            follow = if (first.contains(EMPTY.token.id)) first | follow else first
          case t : TerminalSymbol =>
            follow = firstMap(t)
        }
      }
    }

    m
  }

  lazy val firstPlusMap : Map[IProduction, BitSet] = productions.map { p =>
    val first = firstOfSymbols(p.right, firstMap)
    (p, if (first.contains(EMPTY.token.id)) first | followMap(p.left) else first)
  }.toMap
}