import scala.util.Random
import scala.collection.mutable
import scala.collection.immutable

object Test extends App {
  def |(a : Any*) = List(a : _*)

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

  val initialEnv = new Env(List(
    VarSlot('+, (args : List[Any]) => args(0).asInstanceOf[Int] + args(1).asInstanceOf[Int]),
    VarSlot('-, (args : List[Any]) => args(0).asInstanceOf[Int] - args(1).asInstanceOf[Int]),
    VarSlot('<, (args : List[Any]) => args(0).asInstanceOf[Int] < args(1).asInstanceOf[Int]),
    VarSlot('println, (args : List[Any]) => println(args)),
    VarSlot('null, null)))

  def eval(exp : Any, env : Env) : Any = {
    exp match {
      case x if x.isInstanceOf[Symbol] => env.get(x.asInstanceOf[Symbol])
      case x if !x.isInstanceOf[List[_]] => x
      case List('define, name, valueExp) => env.define(name.asInstanceOf[Symbol], eval(valueExp, env))
      case List('set, name, valueExp) => env.set(name.asInstanceOf[Symbol], eval(valueExp, env))
      case List('begin, exps@_*) => exps.foldLeft(null : Any) { case (last, exp) => eval(exp, env) }
      case List('if, condExp, thenExp, elseExp) => eval(if (eval(condExp, env).asInstanceOf[Boolean]) thenExp else elseExp, env)
      case List('function, List(argNames@_*), body) => { argValues => eval(body, env.extendWith(argNames.map(_.asInstanceOf[Symbol]).toList, argValues)) } : (List[Any] => Any)
      case List(f, actuals@_*) => eval(f, env).asInstanceOf[List[Any] => Any](actuals.map(exp => eval(exp, env)).toList)
    }
  }

  eval(
    |('begin,
      // fib
      |('define, 'fib,
        |('function,
          |('n),
          |('if, |('<, 'n, 2),
            1,
            |('+,
              |('fib, |('-, 'n, 1)),
              |('fib, |('-, 'n, 2)))))),
      // iterate
      |('define, 'iterate,
        |('function,
          |('begin, 'end, 'f),
          |('if, |('<, 'begin, 'end),
            |('begin,
              |('f, 'begin),
              |('iterate, |('+, 'begin, 1), 'end, 'f)),
            'null))),
      // counter
      |('define, 'counter,
        |('function,
          |('n),
          |('function,
            |(),
            |('begin,
              |('set, 'n, |('+, 'n, 1)),
              'n)))),
      // main
      |('iterate, 0, 10,
        |('function,
          |('i),
          |('println, |('fib, 'i)))),
      |('define, 'newCounter, |('counter, 1000)),
      |('println, |('newCounter)),
      |('println, |('newCounter)),
      |('println, |('newCounter))),
    initialEnv)
}
