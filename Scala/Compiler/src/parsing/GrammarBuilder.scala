package parsing

import lexical.Token

import scala.collection.immutable

object Associativity extends Enumeration {
  val Left, Right = Value
}
case class TerminalSymbolAttribute(priority : Int, associativity : Associativity.Value)

case class ErrorRecoveryAction(targetNonTerm : String, action : Any => Any, consumeSyncWord : Boolean)

abstract class GrammarBuilder {

  def start : INonTerminalSymbol

  implicit def token2TerminalSymbol[T](token : T)(implicit ev1 : T => Token) : TerminalSymbol = TerminalSymbol(token)
  implicit def token2GrammarExpr[T](token : T)(implicit ev1 : T => Token) : IGrammarExpr[Any] = token2TerminalSymbol(token)
  implicit def grammarSymbol2GrammarExpr[T](symbol : IGenericGrammarSymbol[T]) : IGrammarExpr[T] = new IGrammarExpr[T] {
    def eval() = List((List(symbol), identity : Action))
  }

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

  def nonTerm[T](tree : => IGrammarExpr[T]) = new GenericNonTerminalSymbol(tree) {
    def name = nonTerm2Name(this)
  }

  def terminalSymbol2Attribute : List[(List[TerminalSymbol], Associativity.Value)] = Nil

  def syncWord2ErrorRecoveryAction : Map[lexical.Token, ErrorRecoveryAction] = Map.empty

  def result : Grammar = new Grammar(
    start,
    terminalSymbol2Attribute.zipWithIndex.flatMap { case ((l, a), i) => l.map((_, TerminalSymbolAttribute(i, a)))}.toMap,
    syncWord2ErrorRecoveryAction)

  def success[T](value : T) : IGrammarExpr[T] = new IGrammarExpr[T] {
    def eval() = List((Nil, { stack =>
      value :: stack
    } : Action))
  }

  def opt[T](expr : IGrammarExpr[T]) : IGrammarExpr[Option[T]] = expr ^^ (Some(_)) | success(None)

  def rep[T](expr : IGrammarExpr[T]) : IGrammarExpr[List[T]] = {
    lazy val nonTerm : GenericNonTerminalSymbol[List[T]] = new GenericNonTerminalSymbol(
      expr ~ nonTerm ^^ { case head ~ tail => head :: tail} | success(Nil)
    ) {
      def name = "rep_" + this.hashCode()
    }
    nonTerm
  }

  def rep1[T](expr : IGrammarExpr[T]) : IGrammarExpr[List[T]] = expr ~ rep(expr) ^^ { case head ~ tail => head :: tail}

  def rep1sep[T, U](expr : IGrammarExpr[T], sep : IGrammarExpr[U]) : IGrammarExpr[List[T]] = expr ~ rep(sep ~> expr) ^^ { case head ~ tail => head :: tail}

  def repsep[T, U](expr : IGrammarExpr[T], sep : IGrammarExpr[U]) : IGrammarExpr[List[T]] = rep1sep(expr, sep) | success(Nil)
}