package parsing

import scala.collection.immutable
import scala.collection.mutable

case class ~[+T, +U](first : T, second : U)

trait IGrammarSymbol
trait IGenericGrammarSymbol[T] extends IGrammarSymbol

case class TerminalSymbol(token : lexical.IToken) extends IGenericGrammarSymbol[Any] with Ordered[TerminalSymbol] {
  override def toString = s"'${token.name}'"
  def compare(that : TerminalSymbol) : Int = token.compare(that.token)
}
object TerminalSymbol {
  val EMPTY = TerminalSymbol(lexical.IToken.Empty)
  val EOF = TerminalSymbol(lexical.IToken.Eof)
  val ERROR = TerminalSymbol(lexical.IToken.Error)
}

trait INonTerminalSymbol extends IGrammarSymbol with Ordered[INonTerminalSymbol] {
  def name : String
  def productions : List[IProduction]
  override def toString = name
  def compare(that : INonTerminalSymbol) : Int = name.compare(that.name)
  var context : Any = null
}
class NonTerminalSymbol(val name : String, var productions : List[IProduction]) extends INonTerminalSymbol
abstract class GenericNonTerminalSymbol[T](tree : => IGrammarTree[T]) extends INonTerminalSymbol with IGenericGrammarSymbol[T] {
  outer =>
  lazy val productions : List[IProduction] = tree.eval().map(p => new Production(outer, p._1, p._2))
}

trait IProduction {
  def left : INonTerminalSymbol
  def right : List[IGrammarSymbol]
  def action : mutable.Stack[Any] => Any
  override def toString = s"$left => ${right.mkString(" ")}"
  def rightHead : IGrammarSymbol = right match {
    case head :: tail => head
    case _ => TerminalSymbol.EMPTY
  }
}
class Production(val left : INonTerminalSymbol, val right : List[IGrammarSymbol], val action : mutable.Stack[Any] => Any) extends IProduction


trait IGrammarTree[+T] {
  outer =>

  type Action = mutable.Stack[Any] => Any
  def eval() : List[(List[IGrammarSymbol], Action)]

  def ^^[R](action : T => R) : IGrammarTree[R] = new IGrammarTree[R] {
    def eval() = outer.eval().map(p => (p._1, { stack =>
      p._2(stack)
      stack.push(action(stack.pop().asInstanceOf[T]))
    } : Action))
  }

  def |[U >: T](other : IGrammarTree[U]) : IGrammarTree[U] = new IGrammarTree[U] {
    def eval() = outer.eval() ::: other.eval()
  }

  def ~[U](other : IGrammarTree[U]) : IGrammarTree[T ~ U] = concat[U, T ~ U](other, (a, b) => new ~(a, b))
  def <~[U](other : IGrammarTree[U]) : IGrammarTree[T] = concat[U, T](other, (a, b) => a)
  def ~>[U](other : IGrammarTree[U]) : IGrammarTree[U] = concat[U, U](other, (a, b) => b)

  private def concat[U, R](other : IGrammarTree[U], f : (T, U) => R) : IGrammarTree[R] = new IGrammarTree[R] {
    def eval() = for (i <- outer.eval();
                      j <- other.eval()) yield (
      i._1 ::: j._1, {
      stack =>
        j._2(stack)
        val b = stack.pop().asInstanceOf[U]
        i._2(stack)
        val a = stack.pop().asInstanceOf[T]
        stack.push(f(a, b))
    } : Action)
  }

  def opt : IGrammarTree[Option[T]] = ^^(Some(_)) | new SuccessGrammarTree(None)

  def rep : IGrammarTree[List[T]] = {
    lazy val nonTerm : GenericNonTerminalSymbol[List[T]] = new GenericNonTerminalSymbol(
      this ~ new SymbolGrammarTree(nonTerm) ^^ { case head ~ tail => head :: tail}
        | new SuccessGrammarTree(Nil)
    ) {
      def name = "rep_" + this.hashCode()
    }

    new SymbolGrammarTree(nonTerm)
  }

  def rep1 : IGrammarTree[List[T]] = this ~ this.rep ^^ { case head ~ tail => head :: tail}

  def rep1sep[U](sep : IGrammarTree[U]) : IGrammarTree[List[T]] = this ~ (sep ~> this).rep ^^ { case head ~ tail => head :: tail}
  def repsep[U](sep : IGrammarTree[U]) : IGrammarTree[List[T]] = rep1sep(sep) | new SuccessGrammarTree(Nil)
}
class SymbolGrammarTree[T](val symbol : IGenericGrammarSymbol[T]) extends IGrammarTree[T] {
  def eval() = List((List(symbol), identity : Action))
}
class SuccessGrammarTree[T](val value : T) extends IGrammarTree[T] {
  def eval() = List((Nil, { stack =>
    stack.push(value)
  } : Action))
}

abstract class GrammarBuilder {
  def start : INonTerminalSymbol

  implicit def token2TerminalSymbol[T <% lexical.IToken](token : T) : TerminalSymbol = TerminalSymbol(token)
  implicit def token2GrammarExpr[T <% lexical.IToken](token : T) : IGrammarTree[Any] = token2TerminalSymbol(token)
  implicit def grammarSymbol2GrammarExpr[T](symbol : IGenericGrammarSymbol[T]) : IGrammarTree[T] = new SymbolGrammarTree(symbol)

  private lazy val nonTerm2Name : immutable.Map[INonTerminalSymbol, String] = {
    import java.lang.reflect.{Method => JMethod}

    val fields = getClass.getDeclaredFields
    def isValDef(m : JMethod) = fields exists (fd => fd.getName == m.getName && fd.getType == m.getReturnType)

    getClass.getMethods.filter(m => m.getParameterTypes.isEmpty &&
      classOf[INonTerminalSymbol].isAssignableFrom(m.getReturnType) &&
      m.getDeclaringClass != classOf[GrammarBuilder] &&
      isValDef(m))
      .map { m =>
      (m.invoke(this).asInstanceOf[INonTerminalSymbol], m.getName)
    }.toMap
  }

  def nonTerm[T](tree : => IGrammarTree[T]) = new GenericNonTerminalSymbol(tree) {
    def name = nonTerm2Name(this)
  }

  def result : Grammar = new Grammar(start)
}

class Grammar(val start : INonTerminalSymbol) {

  import immutable.BitSet

  override def toString = s"start=$start\n\t${productions.mkString("\n\t")}\n"

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
              var values : List[Any] = Nil
              for (_ <- 0 until tail.length) values = stack.pop :: values
              pj.action(stack)
              stack.pushAll(values)

              p.action(stack)
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
              new Production(nnt, Nil, { stack =>
                stack.push(Nil)
              }),
              new Production(nnt, pl.right.tail ::: List(nnt), { stack =>
                val ll = stack.pop().asInstanceOf[List[List[Any]]]
                var values : List[Any] = Nil
                for (_ <- 0 until pl.right.tail.length) values = stack.pop :: values
                stack.push(values :: ll)
              }))

            ps = ps.filter(_ != pl).map(p => new Production(i, p.right ::: List(nnt), { stack =>
              val ll = stack.pop().asInstanceOf[List[List[Any]]]
              p.action(stack)
              for (l <- ll) {
                stack.pushAll(l)
                pl.action(stack)
              }
            }))

            id += 1
            iterate()
          case None =>
        }
      }
      iterate()

      i.productions = ps
    }

    new Grammar(newNonTerms.find(_.name == start.name).get)
  }

  def leftFactoring() : Grammar = {
    val newNonTerms = mutableNonTerms()

    def iterate(nt : NonTerminalSymbol) : Unit = {
      var id = 0
      nt.productions = nt.productions.groupBy(_.rightHead).map {
        case (_, List(p)) => p
        case (_, ps) =>
          var len = 1
          while (len < ps.head.right.length && ps.forall(p => len < p.right.length && p.right(len) == ps.head.right(len))) {
            len += 1
          }

          val nnt = new NonTerminalSymbol(s"${nt.name}_lf$id", Nil)
          nnt.productions = ps.map { p => new Production(nnt, p.right.drop(len), stack => stack.push(p.action))}

          iterate(nnt)

          id += 1
          new Production(nt, ps.head.right.take(len) ::: List(nnt), { stack =>
            val a = stack.pop().asInstanceOf[mutable.Stack[Any] => Any]
            a(stack)
          })
      }.toList
    }
    newNonTerms.foreach(iterate)

    new Grammar(newNonTerms.find(_.name == start.name).get)
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