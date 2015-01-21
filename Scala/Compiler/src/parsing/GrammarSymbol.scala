package parsing

case class ~[+T, +U](first : T, second : U)

trait IGrammarSymbol
trait IGenericGrammarSymbol[T] extends IGrammarSymbol

case class TerminalSymbol(token : lexical.Token) extends IGenericGrammarSymbol[Any] with Ordered[TerminalSymbol] {
  override def toString = s"'${token.name}'"
  def compare(that : TerminalSymbol) : Int = token.compare(that.token)
}
object TerminalSymbol {
  val EMPTY = TerminalSymbol(lexical.Token.EMPTY)
  val EOF = TerminalSymbol(lexical.Token.EOF)
  val ERROR = TerminalSymbol(lexical.Token.ERROR)
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
  lazy val productions : List[IProduction] = expr.eval().map(p => new Production(this, p._1, p._2))
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