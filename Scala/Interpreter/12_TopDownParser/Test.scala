
import scala.collection.mutable
import scala.collection.immutable

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

    def removeRedundantNonTerms() : Grammar = {
      val redundants = nonTerms.toSet -- (productions.flatMap(p => p.body.collect { case t : NonTerm => t}).toSet + startSymbol)
      new Grammar(startSymbol, productions.filter(p => !redundants(p.nonTerm)))
    }

    lazy val nonTerms : List[NonTerm] = {
      productions.flatMap(p => p.nonTerm :: p.body.collect { case t : NonTerm => t}).distinct.sorted
    }

    lazy val terms : List[Term] = {
      (Eof :: Empty :: productions.flatMap(p => p.body.collect { case t : Term => t})).distinct
    }

    override def toString = s"start: $startSymbol\n" + productions.map(_.toString).mkString("\n") + "\n"

    def removeLeftRecursion() : Grammar = {
      var nonTerm2Productions = productions.groupBy(p => p.nonTerm)
      for (i <- nonTerms) {
        for (j <- nonTerms if j < i) {

          def expandJ(productions : List[Production]) : List[Production] = productions match {
            case Nil => Nil
            case Production(_, bodyHead :: bodyTail) :: tail if bodyHead == j => {
              nonTerm2Productions(j).map(p => new Production(i, p.body ::: bodyTail)) ::: expandJ(tail)
            }
            case head :: tail => head :: expandJ(tail)
          }
          nonTerm2Productions = nonTerm2Productions.updated(i, expandJ(nonTerm2Productions(i)))
        }

        var id = 0
        def iterate() : Unit = {
          val productions = nonTerm2Productions(i)
          productions.find(_.body.head == i) match {
            case None =>
            case Some(p) => {
              val newNonTerm = NonTerm(s"${i.name}_rr$id")
              id += 1
              nonTerm2Productions = nonTerm2Productions.updated(newNonTerm,
                List(new Production(newNonTerm, List(Empty)),
                  new Production(newNonTerm, p.body.tail ::: List(newNonTerm))))
              nonTerm2Productions = nonTerm2Productions.updated(i, productions.filter(_ != p).map(p2 => new Production(i, p2.body ::: List(newNonTerm))))
              iterate()
            }
          }
        }
        iterate()
      }

      new Grammar(startSymbol, nonTerm2Productions.values.flatMap(identity).toList).removeRedundantNonTerms()
    }

    def leftFactoring() : Grammar = {
      var id = 0
      def factoring(nonTerm : NonTerm, productions : List[Production]) : List[(NonTerm, List[Production])] = {
        var result : List[(NonTerm, List[Production])] = Nil
        (nonTerm, productions.groupBy(_.body.head).iterator.map {
          case (_, List(p)) => p
          case (_, ps) => {
            var len = 1
            while (ps.head.body.length > len && ps.forall(p => p.body.length > len && p.body(len + 1) == ps.head.body(len + 1))) {
              len += 1
            }

            val newNonTerm = NonTerm(s"${nonTerm.name}_lf$id")
            id += 1
            result = factoring(newNonTerm, ps.map(p => new Production(newNonTerm, p.bodyDrop(len)))) ::: result

            new Production(nonTerm, ps.head.body.take(len) ::: List(newNonTerm))
          }
        }.toList) :: result
      }

      new Grammar(startSymbol, productions.groupBy(_.nonTerm).iterator.flatMap(p => factoring(p._1, p._2)).flatMap(_._2).toList).removeRedundantNonTerms()
    }

    def generatePredicateTable() : PredicateTable = {
      import immutable.BitSet

      val term2ID = terms.zipWithIndex.toMap
      val EmptyID = term2ID(Empty)
      val firstSet = mutable.Map[GrammarSymbol, BitSet](terms.map(t => (t, BitSet(term2ID(t)))) : _*)
      val followSet = mutable.Map[NonTerm, BitSet]()
      val firstPlusSet = mutable.Map[Production, BitSet]()
      val nonTerm2Productions = productions.groupBy(_.nonTerm)

      def firstOfSymbols(symbols : Seq[GrammarSymbol]) : BitSet = symbols match {
        case Nil => firstSet(Empty)
        case head :: tail =>
          val fs = firstSet.getOrElse(head, BitSet.empty)
          if (fs.contains(EmptyID)) (fs - EmptyID) | firstOfSymbols(tail) else fs
      }
      def buildFirstSet() : Unit = {
        var changed = false

        for (nonTerm <- nonTerms) {
          val first = nonTerm2Productions(nonTerm).foldLeft(BitSet.empty) { (s, p) => s | firstOfSymbols(p.body)}
          val oldFirst = firstSet.getOrElse(nonTerm, BitSet.empty)
          if (first != oldFirst) {
            firstSet(nonTerm) = first
            changed = true
          }
        }

        if (changed) buildFirstSet()
      }
      def buildFollowSet() : Unit = {
        var changed = false

        for (p <- productions) {
          var follow = followSet.getOrElse(p.nonTerm, BitSet.empty)
          for (symbol <- p.body.reverse) {
            symbol match {
              case nt : NonTerm => {
                val oldFollow = followSet.getOrElse(nt, BitSet.empty)
                val newFollow = oldFollow | follow
                if (newFollow != oldFollow) changed = true
                followSet(nt) = newFollow
                val fs = firstSet(nt)
                follow = if (fs.contains(EmptyID)) (fs - EmptyID) | follow else fs
              }
              case _ => follow = firstSet(symbol)
            }
          }
        }

        if (changed) buildFollowSet()
      }
      def buildFirstPlusSet() : Unit = {
        for (p <- productions) {
          val first = firstOfSymbols(p.body)
          firstPlusSet(p) = if (first.contains(EmptyID)) (first - EmptyID) | followSet(p.nonTerm) else first
        }
      }

      buildFirstSet()
      buildFollowSet()
      buildFirstPlusSet()

      new PredicateTable(
        startSymbol,
        for ((nt, ps) <- nonTerm2Productions) yield {
          (nt, terms.map { t =>
            val tid = term2ID(t)
            (t, ps.filter(firstPlusSet(_).contains(tid)))
          }.toMap)
        })
    }

    def generateOriginTable() : PredicateTable = {
      new PredicateTable(
        startSymbol,
        productions.groupBy(_.nonTerm).map { case ((nonTerm, ps)) =>
          (nonTerm, terms.map((_, ps)).toMap)
        })
    }
  }

  class PredicateTable(startSymbol : NonTerm, table : Map[NonTerm, Map[Term, List[Production]]]) {
    lazy val isBacktrackFree = table.forall(st => st._2.forall(_._2.length <= 1))

    def print() : Unit = {
      println(s"backtrackFree=$isBacktrackFree")
      table.foreach { case ((nt, row)) =>
        println(s"$nt: (predicative=${row.forall(_._2.length <= 1)})")
        row.keys.groupBy(row(_)).filter(_._1 != Nil).foreach { case ((ps, ts)) =>
          println(s"\t${ts.mkString(",")}=>\n\t\t${ps.mkString("\n\t\t")}\n")
        }
      }
    }

    def parse(source : Seq[Term]) : String = {
      def iterateSymbols(source : Seq[Term], symbols : Seq[GrammarSymbol]) : Stream[Seq[Term]] = symbols match {
        case Nil => Stream(source)
        case head :: tail => iterate(source, head).flatMap(s => iterateSymbols(s, tail))
      }
      def iterate(source : Seq[Term], symbol : GrammarSymbol) : Stream[Seq[Term]] = {
        symbol match {
          case Empty => Stream(source)
          case _ if source.isEmpty => Stream()
          case t : Term if t == source.head => Stream(source.tail)
          case t : Term => Stream()
          case nt : NonTerm => table(nt)(source.head).toStream.flatMap(p => iterateSymbols(source, p.body))
        }
      }

      iterate(source, startSymbol).filter(_.isEmpty).headOption match {
        case None => "Failed"
        case _ => "Success"
      }
    }

    def predicateParse(source : Seq[Term]) : String = {
      assert(isBacktrackFree)
      def iterate(source : Seq[Term], symbol : GrammarSymbol) : Seq[Term] = {
        if (source == null) return null
        symbol match {
          case Empty => source
          case _ if source.isEmpty => null
          case t : Term if t == source.head => source.tail
          case t : Term => null
          case nt : NonTerm => {
            val ps = table(nt)(source.head)
            if (ps.isEmpty) null else ps.head.body.foldLeft(source)(iterate)
          }
        }
      }

      iterate(source, startSymbol) match {
        case null => "Failed"
        case res if !res.isEmpty => "Failed: Too long"
        case _ => "Success"
      }
    }
  }

  //----------------------------------------------------------------------------
  implicit def str2Term(s : String) : Term = Term(s)

  implicit def symbol2NonTerm(s : Symbol) : NonTerm = NonTerm(s.toString().substring(1))

  def buildGrammar() : Grammar = {
    new Grammar('program)
      .rule('program)(
        'exp ~ Eof
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
    "IDENT", "+", "(", "IDENT", "[", "NUMBER", "]", "-", "NUMBER", "*", "IDENT", ")", Eof
  )
  val illegalInput = List[Term](
    "IDENT", "+", "(", "IDENT", "[", "NUMBER", ")", "-", "NUMBER", "*", "IDENT", ")", Eof
  )

  val g = buildGrammar()
  println(g)
  val gt = g.generatePredicateTable()
  gt.print()

  val rrg = g.removeLeftRecursion()
  println(rrg)
  val rrgt = rrg.generatePredicateTable()
  rrgt.print()
  val orgt = rrg.generateOriginTable()
  println(rrgt.parse(input))
  println(rrgt.parse(illegalInput))
  println(orgt.parse(input))
  println(orgt.parse(illegalInput))

  val lfg = rrg.leftFactoring()
  println(lfg)
  val lfgt = lfg.generatePredicateTable()
  lfgt.print()
  println(lfgt.parse(input))
  println(lfgt.parse(illegalInput))
  println(lfgt.predicateParse(input))
  println(lfgt.predicateParse(illegalInput))

  val profileInput = List[Term](
    "IDENT", "+", "(", "IDENT", "[", "NUMBER", "]", "-", "NUMBER", "*", "IDENT", ")", "*", "NUMBER", "/", "IDENT", "+", "NUMBER", "*", "IDENT", "(", "IDENT", ")", "+", "(", "IDENT", "[", "NUMBER", "]", "-", "NUMBER", "*", "IDENT", ")", "*", "NUMBER", "/", "IDENT", "+", "NUMBER", "*", "IDENT", "(", "IDENT", ")", Eof
  )
  println("----------------------------------")
  println(orgt.parse(profileInput))
  println(rrgt.parse(profileInput))
  println(lfgt.parse(profileInput))
  println(lfgt.predicateParse(profileInput))
  Utils.timeit("cfg grammar", 5) {
    for (i <- 0 until 3000) orgt.parse(profileInput)
  }
  Utils.timeit("backtrack grammar", 5) {
    for (i <- 0 until 3000) rrgt.parse(profileInput)
  }
  Utils.timeit("backtrack-free grammar", 5) {
    for (i <- 0 until 3000) lfgt.parse(profileInput)
  }
  Utils.timeit("backtrack-free grammar", 5) {
    for (i <- 0 until 3000) lfgt.predicateParse(profileInput)
  }
}
