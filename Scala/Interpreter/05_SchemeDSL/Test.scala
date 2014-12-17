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
    sealed abstract class Exp
    case class Var(name : Symbol) extends Exp
    case class Const(value : Any) extends Exp
    case class Define(name : Symbol, valueExp : Exp) extends Exp
    case class Set(name : Symbol, valueExp : Exp) extends Exp
    case class Begin(exps : List[Exp]) extends Exp
    case class If(condExp : Exp, thenExp : Exp, elseExp : Exp) extends Exp
    case class Func(argNames : List[Symbol], bodyExp : Exp) extends Exp
    case class Call(funcExp : Exp, argExps : List[Exp]) extends Exp

    implicit def symbol2Var(name : Symbol) = Var(name)
    implicit def int2Const(i : Int) = Const(i)
    implicit def symbol2Extension(name : Symbol) = new {
      def @=(value : Exp) = Define(name, value)
      def :=(value : Exp) = Set(name, value)
    }
    implicit def exp2Extension[T <% Exp](v : T) = new {
      def apply(args : Exp*) = Call(v, args.toList)
      def -(o : Exp) = Call('-, List(v, o))
      def +(o : Exp) = Call('+, List(v, o))
      def <(o : Exp) = Call('<, List(v, o))
    }
    def func(args : Symbol*)(body : Exp*) = Func(args.toList, Begin(body.toList))
    def _if(condExp : Exp)(thenExp : Exp*) = new If(condExp, Begin(thenExp.toList), 'null) {
      def _else(elseExp : Exp*) = If(this.condExp, this.thenExp, Begin(elseExp.toList))
    }
    def program(exps : Exp*) = Begin(exps.toList)
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
      case Begin(exps) => exps.foldLeft(null : Any) { case (_, exp) => eval(exp, env) }
      case If(condExp, thenExp, elseExp) => eval(if (eval(condExp, env).asInstanceOf[Boolean]) thenExp else elseExp, env)
      case Func(argNames, bodyExp) => { argValues => eval(bodyExp, env.extendWith(argNames, argValues)) } : (List[Any] => Any)
      case Call(funcExp, argExps) => eval(funcExp, env).asInstanceOf[List[Any] => Any](argExps.map(exp => eval(exp, env)))
    }
  }

  def main() {
    import AST._

    eval(
      program(
        // fib
        'fib @=
          func('n) {
            _if('n < 2) {
              1
            } _else {
              'fib('n - 1) + 'fib('n - 2)
            }
          },
        // iterate
        'iterate @=
          func('begin, 'end, 'f) {
            _if('begin < 'end)(
              'f('begin),
              'iterate('begin + Const(1), 'end, 'f))
          },
        // counter
        'counter @=
          func('n) {
            func()(
              'n := 'n + Const(1),
              'n)
          },
        // main
        'iterate(0, 10,
          func('i) {
            'println('fib('i))
          }),
        'newCounter @= 'counter(1000),
        'println('newCounter()),
        'println('newCounter()),
        'println('newCounter())),
      initialEnv)
  }

  Utils.timeit("main", 1) {
    main()
  }
}
