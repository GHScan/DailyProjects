import scala.util.Random
import scala.collection.mutable
import scala.collection.immutable

class Vector(val values : Seq[BigInt]) extends AnyVal {
  def *(v : Vector) = values.zip(v.values).foldLeft(0 : BigInt) { case (r, (i, j)) => r + i * j }
}
class Matrix(val values : Seq[Seq[BigInt]]) {
  def *(v : Vector) : Vector = new Vector(values.map(new Vector(_) * v))
  def *(m : Matrix) : Matrix = { val tm = m.transpose; new Matrix(values.map(v => (tm * new Vector(v)).values)) }
  def **(n : Int) : Matrix = {
    n match {
      case 0 => identity
      case x if x % 2 == 1 => this * (this ** (n / 2)).double
      case _ => (this ** (n / 2)).double
    }
  }
  def double = this * this
  def identity = new Matrix(
    for ((r, i) <- values.zipWithIndex)
      yield for ((v, j) <- r.zipWithIndex)
      yield if (i == j) 1 : BigInt else 0 : BigInt)
  def transpose = new Matrix((0 until values(0).length).map(i => values.map(_(i))))
}

object Test extends App {
  def fib(n : Int) : BigInt = {
    val m = new Matrix(List(List(0, 1), List(1, 1))) ** (n)
    val v = m * new Vector(List(0, 1))
    return v.values(0)
  }

  val fibs : Stream[BigInt] = 1 #:: 1 #:: fibs.zip(fibs.drop(1)).map { case (i, j) => i + j }

  //  (0 until 5) map fib foreach println
  //  fibs take 5 print

  val len = 100
  val kth = 100000
  
  Utils.timeit(s"fibs len=$len", 1) {
    fibs take len toList
  }
  Utils.timeit(s"fib len=$len", 5) {
    (0 until len) map fib toList
  }
  
  Utils.timeit(s"fibs kth=$kth", 1) {
    fibs drop kth head
  }
  Utils.timeit(s"fib kth=$kth'", 5) {
    fib(kth)
  }
}
