package utils

object Func {

  def escape(_s : String) : String = {
    import scala.reflect.runtime.universe._
    Literal(Constant(_s)).toString()
  }

  def unescape(s : String) : String = {
    val sb = new StringBuilder()
    var i = 1
    while (i < s.length - 1) {
      s(i) match {
        case '\\' =>
          i += 1
          s(i) match {
            case 't' => sb += '\t'
            case 'r' => sb += '\r'
            case 'n' => sb += '\n'
            case c => sb += c
          }
        case c => sb += c
      }
      i += 1
    }
    sb.toString()
  }

  def round2PowerOf2(i : Int) : Int = {
    1 << math.ceil(math.log(i) / math.log(2)).toInt
  }

  def splitToContinuesSegments[T](l : List[T])(implicit integral : Integral[T]) : List[List[T]] = {
    import integral._

    def iterate(last : List[T], l : List[T]) : List[List[T]] = l match {
      case Nil => List(last.reverse)
      case head :: tail if head == last.head + one => iterate(head :: last, tail)
      case head :: tail => last.reverse :: iterate(List(head), tail)
    }
    l match {
      case Nil => Nil
      case head :: tail => iterate(List(head), tail)
    }
  }
}