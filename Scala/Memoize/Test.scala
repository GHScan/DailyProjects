import scala.util.Random
import scala.collection.mutable
import scala.collection.immutable

object Test extends App {
  def memoize[In, Out](f : In => Out) : In => Out = {
    val m = mutable.Map[In, Out]()
    v => m.getOrElseUpdate(v, f(v))
  }

  val fib : Int => BigInt = memoize(n => {
    n match {
      case x if x < 2 => 1
      case _ => fib(n - 1) + fib(n - 2)
    }
  })
  println((0 until 10) map fib mkString (","))

  val shortestPalindrome : String => String = memoize(s => {
    s match {
      case x if x.length < 2 => x
      case x if x.head == x.last => x.head + shortestPalindrome(x.substring(1, x.length - 1)) + x.head
      case x => {
        val left = shortestPalindrome(x.substring(0, x.length - 1))
        val right = shortestPalindrome(x.substring(1))
        if (left < right) x.last + left + x.last else x.head + right + x.head
      }
    }
  })
  println(shortestPalindrome("fesse"))
  println(shortestPalindrome("cbabd"))
}
