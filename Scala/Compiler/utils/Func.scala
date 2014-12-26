package utils

object Func {

  def escape(s : String) : String = {
    import scala.reflect.runtime.universe._
    Literal(Constant(s)).toString
  }

}