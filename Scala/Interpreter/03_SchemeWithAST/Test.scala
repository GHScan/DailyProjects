import scala.util.Random
import scala.collection.mutable
import scala.collection.immutable

object Test extends App {

  case class VarSlot(val name : Symbol, var value : Any)
  class Env(var vars : List[VarSlot]) {
    def extendWith(names : List[Symbol], values : List[Any]) = {
      new Env(names.zip(values).map { case (name, value) => VarSlot(name, value) } ++ vars)
    }
    def define(name : Symbol, value : Any) = vars = VarSlot(name, value) :: vars
    def set(name : Symbol, value : Any) = vars.find(_.name == name) match {
      case Some(v) => v.value = value
      case None => throw new Exception(s"Set failed, invalid name : $name")
    }
    def get(name : Symbol) = vars.find(_.name == name) match {
      case Some(v) => v.value
      case None => throw new Exception(s"Lookup failed, invalid name : $name")
    }
  }

  object AST {
    abstract class Exp
    case class Var(name : Symbol) extends Exp
    case class Const(value : Any) extends Exp
    case class Define(name : Symbol, valueExp : Exp) extends Exp
    case class Set(name : Symbol, valueExp : Exp) extends Exp
    case class Begin(exps : Exp*) extends Exp
    case class If(condExp : Exp, thenExp : Exp, elseExp : Exp) extends Exp
    case class Func(argNames : List[Symbol], bodyExp : Exp) extends Exp
    case class Call(funcExp : Exp, argExps : Exp*) extends Exp
  }

  val initialEnv = new Env(List(
    VarSlot('+, (args : List[Any]) => args(0).asInstanceOf[Int] + args(1).asInstanceOf[Int]),
    VarSlot('-, (args : List[Any]) => args(0).asInstanceOf[Int] - args(1).asInstanceOf[Int]),
    VarSlot('<, (args : List[Any]) => args(0).asInstanceOf[Int] < args(1).asInstanceOf[Int]),
    VarSlot('println, (args : List[Any]) => println(args)),
    VarSlot('null, null)))

  def eval(exp : AST.Exp, env : Env) : Any = {
    import AST._
    exp match {
      case Var(name) => env.get(name)
      case Const(value) => value
      case Define(name, valueExp) => env.define(name, eval(valueExp, env))
      case Set(name, valueExp) => env.set(name, eval(valueExp, env))
      case Begin(exps@_*) => exps.foldLeft(null : Any) { case (_, exp) => eval(exp, env) }
      case If(condExp, thenExp, elseExp) => eval(if (eval(condExp, env).asInstanceOf[Boolean]) thenExp else elseExp, env)
      case Func(argNames, bodyExp) => { argValues => eval(bodyExp, env.extendWith(argNames, argValues)) } : (List[Any] => Any)
      case Call(funcExp, argExps@_*) => eval(funcExp, env).asInstanceOf[List[Any] => Any](argExps.map(exp => eval(exp, env)).toList)
    }
  }

  def main() {
    import AST._
    eval(
      Begin(
        // fib
        Define('fib,
          Func(List('n),
            If(Call(Var('<), Var('n), Const(2)),
              Const(1),
              Call(Var('+),
                Call(Var('fib), Call(Var('-), Var('n), Const(1))),
                Call(Var('fib), Call(Var('-), Var('n), Const(2))))))),
        // iterate
        Define('iterate,
          Func(List('begin, 'end, 'f),
            If(Call(Var('<), Var('begin), Var('end)),
              Begin(
                Call(Var('f), Var('begin)),
                Call(Var('iterate), Call(Var('+), Var('begin), Const(1)), Var('end), Var('f))),
              Var('null)))),
        // counter
        Define('counter,
          Func(List('n),
            Func(List(),
              Begin(
                Set('n, Call(Var('+), Var('n), Const(1))),
                Var('n))))),
        // main
        Call(Var('iterate), Const(0), Const(10),
          Func(List('i),
            Call(Var('println), Call(Var('fib), Var('i))))),
        Define('newCounter, Call(Var('counter), Const(1000))),
        Call(Var('println), Call(Var('newCounter))),
        Call(Var('println), Call(Var('newCounter))),
        Call(Var('println), Call(Var('newCounter)))),
      initialEnv)
  }

  main()
}
