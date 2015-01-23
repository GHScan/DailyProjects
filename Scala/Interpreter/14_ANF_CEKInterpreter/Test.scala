
class SExprParser extends scala.util.parsing.combinator.RegexParsers {
  private val list : Parser[List[Any]] = "(" ~> rep(value) <~ ")" | "[" ~> rep(value) <~ "]"
  private lazy val value : Parser[Any] = (
    """\d+""".r ^^ (_.toInt)
      | """[^()\[\]\s]+""".r
      | list)
  def parse(input : String) : Any = {
    val input2 = input.replaceAll( """[;#][^\n]*""", "")
    val result = parseAll(rep(value), input2)
    if (result.successful) List(List("lambda", List(), "begin" :: result.get))
    else throw new Exception(result.toString)
  }
}

object SExpr {

  def toString(v : Any) : String = v match {
    case l : List[_] => s"(${l.map(toString).mkString(" ")})"
    case _ => v.toString
  }

  def print(v : Any) : Unit = {
    println(toString(v))
  }

  def atom(v : Any) : Boolean = v match {
    case _ : Int | _ : String => true
    case _ => false
  }

  def desugar(v : Any) : Any = {
    // 1. remove cond
    // 2. merge multiple expr to begin
    def reduceComplexForm(v : Any) : Any = v match {
      case _ if atom(v) => v
      case List("cond", List("else", caseBody@_*)) =>
        reduceComplexForm("begin" :: caseBody.toList)
      case List("cond", List(caseCond, caseBody@_*), restCase@_*) =>
        List("if", reduceComplexForm(caseCond),
          reduceComplexForm("begin" :: caseBody.toList),
          reduceComplexForm("cond" :: restCase.toList))
      case List("cond", _*) => sys.error("Illegal: " + v.toString)
      case List("define", List(name : String, formals@_*), body@_*) =>
        reduceComplexForm(List("define", name, List("lambda", formals, "begin" :: body.toList)))
      case List("lambda", formals, body@_*) if body.length > 1 =>
        reduceComplexForm(List("lambda", formals, "begin" :: body.toList))
      case List("let", bounds, body@_*) if body.length > 1 =>
        reduceComplexForm(List("let", bounds, "begin" :: body.toList))
      case List("begin", nested@List("begin", _*)) =>
        reduceComplexForm(nested)
      case List("begin", expr) => reduceComplexForm(expr)
      case l : List[_] => l.map(reduceComplexForm)
    }
    def removeDefine(v : Any) : Any = {
      def removeDefineForBegin(l : List[Any]) : List[Any] = {
        val (defines, others) = l.partition {
          case List("define", _*) => true
          case _ => false
        }
        if (defines.nonEmpty) {
          List(
            List("let",
              defines.map { case d : List[_] => List(d(1), "void")},
              "begin" :: defines.map { case d : List[_] => List("set!", d(1), d(2))} ::: others))
        } else {
          others
        }
      }

      v match {
        case _ if atom(v) => v
        case List("begin", exprs@_*) => "begin" :: removeDefineForBegin(exprs.toList).map(removeDefine)
        case l : List[_] => l.map(removeDefine)
      }
    }
    def removeBegin(v : Any) : Any = v match {
      case _ if atom(v) => v
      case List("begin", exprs@_*) =>
        def iterate(exprs : List[Any]) : Any = exprs match {
          case List(expr) => expr
          case head :: tail => List("let", List(List("_", head)), iterate(tail))
        }
        removeBegin(iterate(exprs.toList))
      case l : List[_] => l.map(removeBegin)
    }
    def removeLet(v : Any) : Any = v match {
      case _ if atom(v) => v
      case List("let", List(), body) => removeLet(body)
      case List("let", List(List(name : String, expr), rest@_*), body) => List(List("lambda", List(name), removeLet(List("let", rest, body))), removeLet(expr))
      case List("let", _*) => sys.error("Illegal: " + v.toString)
      case l : List[_] => l.map(removeLet)
    }

    val pipeline = List(reduceComplexForm _, removeDefine _, removeBegin _, removeLet _)
    pipeline.foldLeft(v) { case (v2, f) => f(v2)}
  }

  private var mNextID = 0
  def genName() : String = {
    mNextID += 1
    s"_gen_$mNextID"
  }

  def anf(v : Any) : Any = {
    val kIdentity : Any => Any = identity
    def normalize2AtomExpr(v : Any, k : Any => Any) : Any = normalize(v, {
      case v2 if atom(v2) => k(v2)
      case v2@List("lambda", _*) => k(v2)
      case v2 =>
        val name = genName()
        List("let", List(List(name, v2)), k(name))
    })
    def normalize2AtomExprList(l : List[Any], k : List[Any] => Any) : Any = l match {
      case Nil => k(Nil)
      case head :: tail =>
        normalize2AtomExpr(head, head2 =>
          normalize2AtomExprList(tail, tail2 =>
            k(head2 :: tail2)))
    }
    def normalize(v : Any, k : Any => Any) : Any = v match {
      case _ if atom(v) => k(v)
      case List("lambda", names, body) => k(List("lambda", names, normalize(body, kIdentity)))
      case List("if", condExpr, thenExpr, elseExpr) =>
        normalize2AtomExpr(condExpr, condExpr2 =>
          k(List("if", condExpr2, normalize(thenExpr, kIdentity), normalize(elseExpr, kIdentity))))
      case List("set!", name : String, expr) =>
        normalize2AtomExpr(expr, expr2 =>
          k(List("set!", name, expr2)))
      case List("quote", expr) => List("quote", normalize(expr, kIdentity))
      case l : List[_] => normalize2AtomExprList(l, k)
    }

    normalize(v, kIdentity)
  }

}

case class MutableTuple2[T, U](var _1 : T, var _2 : U)

object Interpreter {
  type Env = List[MutableTuple2[String, Any]]
  type FuncT = List[_] => Any

  case class Closure(lambda : Any, env : Env)

  val InitialEnv =
    (List[(String, Any)](
      "true" -> true, "false" -> false, "void" -> null, "empty" -> Nil)
      ::: List[(String, FuncT)](
      "+" -> { case List(a : Int, b : Int) => a + b},
      "-" -> { case List(a : Int, b : Int) => a - b},
      "*" -> { case List(a : Int, b : Int) => a * b},
      "/" -> { case List(a : Int, b : Int) => a / b},
      "quotient" -> { case List(a : Int, b : Int) => a / b},
      "remainder" -> { case List(a : Int, b : Int) => a % b},
      "sqr" -> { case List(a : Int) => a * a},
      "sqrt" -> { case List(a : Int) => math.sqrt(a).toInt},
      "identity" -> { case List(a : Any) => a},
      "=" -> { case List(a : Any, b : Any) => a.equals(b)},
      "not" -> { case List(a : Boolean) => !a},
      "<" -> { case List(a : Int, b : Int) => a < b},
      "<=" -> { case List(a : Int, b : Int) => a <= b},
      ">" -> { case List(a : Int, b : Int) => a > b},
      ">=" -> { case List(a : Int, b : Int) => a >= b},
      "=" -> { case List(a : AnyRef, b : AnyRef) => a.eq(b)},
      "cons" -> { case List(a : Any, b : List[_]) => a :: b},
      "car" -> { case List(a :: b) => a},
      "cdr" -> { case List(a :: b) => b},
      "cadr" -> { case List(a :: b :: c) => b},
      "caddr" -> { case List(a :: b :: c :: d) => c},
      "cadddr" -> { case List(a :: b :: c :: d :: e) => d},
      "drop" -> { case List(a : List[_], b : Int) => a.drop(b)},
      "append" -> { case List(a : List[_], b : List[_]) => a ::: b},
      "length" -> { case List(a : List[_]) => a.length},
      "empty?" -> { case List(a : List[_]) => a.isEmpty},
      "pretty-print" -> { case List(a : Any) => SExpr.print(a)},
      "display" -> { case List(a : Any) => print(SExpr.toString(a))},
      "current-inexact-milliseconds" -> { case List() => System.currentTimeMillis().toInt; case v => sys.error(s"Illegal $v")},
      "random" -> { case List(n : Int) => (math.random * n).toInt}
    ))
}

class ANF_CEKInterpreter {

  import Interpreter._

  private type Continuation = Any => List[IState]
  private type SuperFuncT = (List[_], Env, Continuation) => List[IState]

  private sealed abstract class IState
  private class TerminateState(val value : Any) extends IState
  private class State(expr : Any, env : Env, k : Continuation) extends IState {
    def valueOfAtomExpr(expr : Any) : Any = expr match {
      case _ : Int => expr
      case name : String => env.find(_._1 == name) match {
        case Some(MutableTuple2(_, value)) => value
        case None => sys.error("Can't find variable: " + name)
      }
      case lambda@List("lambda", _*) => Closure(lambda, env)
    }
    def derive() : List[IState] = expr match {
      case List("set!", name : String, valueExpr) =>
        env.find(_._1 == name) match {
          case Some(tuple : MutableTuple2[String, Any]) => tuple._2 = valueOfAtomExpr(valueExpr)
          case None => sys.error("Can't find variable: " + name)
        }
        k(null)
      case List("let", List(List(name : String, valueExpr)), body) => List(new State(valueExpr, env, value => List(new State(body, MutableTuple2(name, value) :: env, k))))
      case List("if", condExpr, thenExpr, elseExpr) => List(new State(if (valueOfAtomExpr(condExpr).asInstanceOf[Boolean]) thenExpr else elseExpr, env, k))
      case List("quote", datum) => k(datum)
      case func :: actuals => valueOfAtomExpr(func) match {
        case Closure(List("lambda", names : List[_], body), env2) =>
          assert(names.length == actuals.length, "Argument count should match")
          List(new State(body, names.asInstanceOf[List[String]].zip(actuals.map(valueOfAtomExpr)).map { case (k2, v) => MutableTuple2(k2, v)} ::: env2, k))
        case f : FuncT => k(f(actuals.map(valueOfAtomExpr)))
        case f : SuperFuncT => f(actuals.map(valueOfAtomExpr), env, k)
      }
      case _ => k(valueOfAtomExpr(expr))
    }
  }

  private val initialEnv = (List[(String, SuperFuncT)](
    "eval" -> { case (List(e : Any), env, k) => k(_eval(e, env))},
    "exit" -> { case (List(), env, k) => Nil; case v => sys.error(s"Illegal $v")},
    "call/cc" -> { case (List(Closure(List("lambda", List(name : String), body), env2)), env, k) =>
      List(new State(body, MutableTuple2(name, ({
        case (List(value), _, _) => k(value)
      } : SuperFuncT) : Any) :: env2, k))
    },
    "fork" -> { case (values, env, k) => values.flatMap(k)}
  ) ::: InitialEnv).map { case (k, v) => MutableTuple2(k, v)}

  private def _eval(expr : Any, env : Env) : Any = {
    def iterate(states : List[IState], result : Any) : Any = states match {
      case Nil => result
      case (head : State) :: tail => iterate(tail ::: head.derive(), result)
      case (head : TerminateState) :: tail => iterate(tail, head.value)
    }
    iterate(List(new State(expr, env, value => List(new TerminateState(value)))), null)
  }

  def eval(expr : Any) : Any = _eval(expr, initialEnv)
}

object Test extends App {
  val parser = new SExprParser()
  val result = parser.parse(scala.io.Source.fromFile("test.rkt").mkString)
  val result2 = SExpr.desugar(result)
  val result3 = SExpr.anf(result2)

  new ANF_CEKInterpreter().eval(result3)
}
