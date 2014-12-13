import scala.util.Random
import scala.collection.mutable
import scala.collection.immutable

object Test extends App {
  import scala.xml.Node

  case class VarSlot(val name : String, var value : Any)
  class Env(var vars : List[VarSlot]) {
    def derive(names : List[String], values : List[Any]) = {
      new Env((names.zip(values).map { case (name, value) => VarSlot(name, value) }) ++ vars)
    }
    def define(name : String, value : Any) = vars = VarSlot(name, value) :: vars
    def set(name : String, value : Any) = vars.find(_.name == name) match {
      case Some(slot) => slot.value = value
      case None => throw new Exception(s"Invalid varaible: $name")
    }
    def lookup(name : String) = vars.find(_.name == name) match {
      case Some(slot) => slot.value
      case None => throw new Exception(s"Invalid varaible: $name")
    }
  }

  val initialEnv = new Env(List[VarSlot](
    VarSlot("less", ({ args => args(0).asInstanceOf[Double] < args(1).asInstanceOf[Double] } : List[Any] => Any)),
    VarSlot("add", ({ args => args(0).asInstanceOf[Double] + args(1).asInstanceOf[Double] } : List[Any] => Any)),
    VarSlot("sub", ({ args => args(0).asInstanceOf[Double] - args(1).asInstanceOf[Double] } : List[Any] => Any)),
    VarSlot("println", ({ args => println(args) } : List[Any] => Any)),
    VarSlot("null", null)))

  implicit def node2Extension(n : Node) = new {
    def attr(name : String) = n.attribute(name) match {
      case Some(n) => n.text
      case None => throw new Exception(s"Invalid attribute: $name")
    }
    def trim() = scala.xml.Utility.trim(n)
  }

  def eval(e : Node, env : Env) : Any = {
    e match {
      case <begin>{ exps@_* }</begin> => exps.foldLeft(null : Any) { case (lastv, exp) => eval(exp, env) }
      case node@ <var/> => env lookup node.attr("name")
      case node@ <number/> => node.attr("value").toDouble
      case node@ <def>{ bodyExp }</def> => env.define(node.attr("name"), eval(bodyExp, env))
      case node@ <set>{ bodyExp }</set> => env.set(node.attr("name"), eval(bodyExp, env))
      case <if>{ condExp }{ thenExp }{ elseExp }</if> => if (eval(condExp, env).asInstanceOf[Boolean]) eval(thenExp, env) else eval(elseExp, env)
      case node@ <function>{ bodyExp }</function> => {
        val argNames = node.attr("args").split(" ").toList;
        { argValues => eval(bodyExp, env.derive(argNames, argValues)) } : (List[Any] => Any)
      }
      case node@ <call>{ args@_* }</call> => {
        env.lookup(node.attr("name")).asInstanceOf[List[Any] => Any](args.map(exp => eval(exp, env)).toList)
      }
      case node if node.isInstanceOf[scala.xml.Comment] => (null, env)
    }
  }

  def main() {
    eval(
      <begin>
        <!-- def fib -->
        <def name="fib">
          <function args="n">
            <if>
              <call name="less"><var name="n"/><number value="2"/></call>
              <number value="1"/>
              <call name="add">
                <call name="fib"><call name="sub"><var name="n"/><number value="1"/></call></call>
                <call name="fib"><call name="sub"><var name="n"/><number value="2"/></call></call>
              </call>
            </if>
          </function>
        </def>
        <!-- def iterate -->
        <def name="iterate">
          <function args="begin end f">
            <if>
              <call name="less"><var name="begin"/><var name="end"/></call>
              <begin>
                <call name="f"><var name="begin"/></call>
                <call name="iterate">
                  <call name="add"><var name="begin"/><number value="1"/></call>
                  <var name="end"/>
                  <var name="f"/>
                </call>
              </begin>
              <var name="null"/>
            </if>
          </function>
        </def>
        <!-- def counter -->
        <def name="counter">
          <function args="n">
            <function args="">
              <begin>
                <set name="n">
                  <call name="add">
                    <var name="n"/>
                    <number value="1"/>
                  </call>
                </set>
                <var name="n"/>
              </begin>
            </function>
          </function>
        </def>
        <!-- main -->
        <call name="iterate">
          <number value="0"/>
          <number value="14"/>
          <function args="i">
            <call name="println">
              <call name="fib">
                <var name="i"/>
              </call>
            </call>
          </function>
        </call>
        <def name="newCounter">
          <call name="counter">
            <number value="10000"/>
          </call>
        </def>
        <call name="println"><call name="newCounter"></call></call>
        <call name="println"><call name="newCounter"></call></call>
        <call name="println"><call name="newCounter"></call></call>
      </begin>.trim(),
      initialEnv)
  }

  main()
}
