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

  type Trampoline = (Any, Any)
  type Continuation = Any => Trampoline;

  val initialEnv = new Env(List(
    VarSlot('+, (args : List[Any], k : Continuation) => (k, args(0).asInstanceOf[Int] + args(1).asInstanceOf[Int])),
    VarSlot('-, (args : List[Any], k : Continuation) => (k, args(0).asInstanceOf[Int] - args(1).asInstanceOf[Int])),
    VarSlot('<, (args : List[Any], k : Continuation) => (k, args(0).asInstanceOf[Int] < args(1).asInstanceOf[Int])),
    VarSlot('==, (args : List[Any], k : Continuation) => (k, args(0).asInstanceOf[Int] == args(1).asInstanceOf[Int])),
    VarSlot('println, (args : List[Any], k : Continuation) => (k, println(args))),
    VarSlot('null, null)))

  def eval(exp : AST.Exp, env : Env) : Any = {
    import AST._

    def evalSeqCPS(exps : Seq[Exp], env : Env, k : Continuation) : Trampoline = {
      if (exps.isEmpty) (k, Nil)
      else evalCPS(exps.head, env, head => evalSeqCPS(exps.tail, env, tail => (k, head :: tail.asInstanceOf[List[Any]])))
    }

    def evalCPS(exp : Exp, env : Env, k : Continuation) : Trampoline = {
      exp match {
        case Var(name) => (k, env.get(name))
        case Const(value) => (k, value)
        case Define(name, valueExp) => evalCPS(valueExp, env, v => (k, env.define(name, v)))
        case Set(name, valueExp) => evalCPS(valueExp, env, v => (k, env.set(name, v)))
        case Begin(exps@_*) => evalSeqCPS(exps, env, values => (k, values.asInstanceOf[List[Any]].last))
        case If(condExp, thenExp, elseExp) =>
          evalCPS(condExp, env, cond =>
            evalCPS(if (cond.asInstanceOf[Boolean]) thenExp else elseExp, env, k))
        case Func(argNames, bodyExp) =>
          (k, (argValues : List[Any], k : Continuation) =>
            evalCPS(bodyExp, env.extendWith(argNames, argValues), k))
        case Call(funcExp, argExps@_*) =>
          evalCPS(funcExp, env, func =>
            evalSeqCPS(argExps, env, args =>
              func.asInstanceOf[(List[Any], Continuation) => Trampoline](args.asInstanceOf[List[Any]], k)))
      }
    }

    def runTrampoline(t : Trampoline) : Any = {
      t match {
        case (k : Continuation, v) => runTrampoline(k(v))
        case (null, v) => v
      }
    }

    runTrampoline(evalCPS(exp, env, null))
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
        // add
        Define('add,
          Func(List('a, 'b),
            If(Call(Var('==), Var('a), Const(0)),
              Var('b),
              Call(Var('add),
                Call(Var('-), Var('a), Const(1)),
                Call(Var('+), Var('b), Const(1)))))),
        // main
        Call(Var('iterate), Const(0), Const(10),
          Func(List('i),
            Call(Var('println), Call(Var('fib), Var('i))))),
        Define('newCounter, Call(Var('counter), Const(1000))),
        Call(Var('println), Call(Var('newCounter))),
        Call(Var('println), Call(Var('newCounter))),
        Call(Var('println), Call(Var('newCounter))),
        Call(Var('println), Call(Var('add), Const(10000), Const(0)))),
      initialEnv)
  }

  Utils.timeit("main", 1) {
    main()
  }
}
