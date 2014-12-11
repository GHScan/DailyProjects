import scala.util.Random
import scala.collection.mutable
import scala.collection.immutable

object Test extends App {
  def Y[T](f : (T => T) => (T => T)) : (T => T) = v => f(Y(f))(v)

  val fib =
    Y[Int](self => n =>
      n match {
        case x if x < 2 => 1
        case _ => self(n - 1) + self(n - 2)
      })
  println((0 until 10) map fib mkString (","))

  val qsort =
    Y[List[Int]](self => l =>
      l match {
        case Nil => Nil
        case head :: tail => self(tail.filter(_ < head)) ::: head :: self(tail.filter(_ > head))
      })
  println(qsort(List(3, 5, 4, 1, 2)))

}
