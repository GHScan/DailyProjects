package parsing

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
final class NonTerminalSymbol(val name : String, var productions : List[IProduction]) extends INonTerminalSymbol
abstract class GenericNonTerminalSymbol[T](expr : => IGrammarExpr[T]) extends INonTerminalSymbol with IGenericGrammarSymbol[T] {
  outer =>
  lazy val productions : List[IProduction] = expr.eval().map(p => new Production(outer, p._1, p._2))
}

trait IProduction {
  def left : INonTerminalSymbol
  def right : List[IGrammarSymbol]
  def action : List[Any] => List[Any]
  override def toString = s"$left => ${right.mkString(" ")}"
  def rightHead : IGrammarSymbol = right match {
    case head :: tail => head
    case _ => TerminalSymbol.EMPTY
  }
}
final class Production(val left : INonTerminalSymbol, val right : List[IGrammarSymbol], val action : List[Any] => List[Any]) extends IProduction
