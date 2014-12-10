import scala.util.Random
import scala.collection.mutable
import scala.collection.immutable

class DifferenceList[T](f : List[T] => List[T]) extends Iterable[T] {
  private[this] lazy val mList = f(Nil)
  
  override def toList = mList
  def iterator = toList.iterator
  def +:(e : T) = new DifferenceList[T](tail => e :: f(tail))
  def :+(e : T) = new DifferenceList[T](tail => f(e :: tail))
}
object DifferenceList {
  def apply[T](args : T*) = args.foldRight(new DifferenceList[T](tail => tail))(_ +: _)
}

object Test extends App {
  val kLen = 1024 * 4
  val kTimes = 10
  
  Utils.timeit("List.+:", kTimes) {
    var a = List(0)
    var i = 0
    while (i < kLen) {
      a = i +: a
      i += 1
    }
    a.size
  }
  Utils.timeit("List.+:", kTimes) {
    var a = List(0)
    var i = 0
    while (i < kLen) {
      a :+= i
      i += 1
    }
    a.size
  }
  Utils.timeit("DifferenceList.+:", kTimes) {
    var a = DifferenceList(0)
    var i = 0
    while (i < kLen) {
      a = i +: a
      i += 1
    }
    a.size
  }
  Utils.timeit("DifferenceList.+:", kTimes) {
    var a = DifferenceList(0)
    var i = 0
    while (i < kLen) {
      a :+= i
      i += 1
    }
    a.size
  }
}
