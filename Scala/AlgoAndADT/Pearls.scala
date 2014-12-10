import scala.util.Random
import scala.collection.mutable
import scala.collection.immutable

object Test extends App {

  def qsort[T <% Ordered[T]](a : List[T]) : List[T] = {
    a match {
      case Nil => Nil
      case head :: tail => qsort(tail.filter(_ < head)) ::: head :: qsort(tail.filter(_ > head))
    }
  }

  def permutation[T](a : List[T]) : List[List[T]] = {
    a match {
      case Nil => List(Nil)
      case head :: tail =>
        for (
          l <- permutation(tail);
          i <- 0 to l.length;
          (left, right) = l.splitAt(i)
        ) yield left ::: head :: right
    }
  }

  def combination[T](a : List[T], n : Int) : List[List[T]] = {
    if (n == 0) return List(Nil)
    else if (a.isEmpty) return Nil
    else combination(a.tail, n) ::: combination(a.tail, n - 1).map(a.head :: _)
  }

  def streamFrom(from : BigInt) : Stream[BigInt] = from #:: streamFrom(from + 1)
  def primesBuilder(q : Stream[BigInt]) : Stream[BigInt] = {
    q match {
      case head #:: tail => head #:: primesBuilder(tail.filter(_ % head != 0))
    }
  }
  val primes = primesBuilder(streamFrom(2))

  def queens(n : Int) : List[List[(Int, Int)]] = {
    def valid(x : Int, y : Int, board : List[(Int, Int)]) = board.forall { case (x2, y2) => x2 != x && math.abs(x - x2) != y2 - y }
    def generate(y : Int) : List[List[(Int, Int)]] = {
      if (y > n) List(Nil)
      else for (
        board <- generate(y + 1);
        x <- 1 to n;
        if valid(x, y, board)
      ) yield (x, y) :: board
    }
    generate(1)
  }

  def rleEncode[T](a : Seq[T]) : List[(T, Int)] = {
    def group(x : T, count : Int, a : Seq[T]) : List[(T, Int)] = {
      if (a.isEmpty) List((x, count))
      else if (a.head == x) group(x, count + 1, a.tail)
      else (x, count) :: group(a.head, 1, a.tail)
    }
    group(a.head, 1, a.tail)
  }
  def rleDecode[T, To](s : List[(T, Int)])(implicit br : scala.collection.generic.CanBuildFrom[Nothing, T, To]) = {
    val b = br()
    s.foreach { case (v, count) => for (i <- 0 until count) b += v }
    b.result
  }

  def main() {
    println(primes.take(10).toList)
  }
  main()
}
