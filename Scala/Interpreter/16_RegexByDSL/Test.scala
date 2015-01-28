object Test extends App {

  sealed abstract class Tree
  case class Chars(chars: List[Char]) extends Tree
  case class Concatenation(first: Tree, second: Tree) extends Tree
  case class Alternation(first: Tree, second: Tree) extends Tree
  case class Repeat(tree: Tree, min: Int, max: Int) extends Tree

  def matchPrefixRepeatly(tree: Tree, input: List[Char], k: List[List[Char]] => Boolean): Boolean = {
    matchPrefix(tree, input, {
      case part1 =>
        matchPrefixRepeatly(tree, input.drop(part1.length), {
          case parts => k(part1 :: parts)
        })
    }) || k(Nil)
  }
  def matchPrefix(tree: Tree, input: List[Char], k: List[Char] => Boolean): Boolean = input match {
    case Nil => false
    case head :: tail => tree match {
      case Chars(chars) => chars.contains(head) && k(List(head))
      case Concatenation(first, second) => matchPrefix(first, input, {
        case part1 => matchPrefix(second, input.drop(part1.length), {
          case part2 => k(part1 ++ part2)
        })
      })
      case Alternation(first, second) => matchPrefix(first, input, {
        case part1 => k(part1)
      }) || matchPrefix(second, input, {
        case part2 => k(part2)
      })
      case Repeat(tree2, min, max) => matchPrefixRepeatly(tree2, input, {
        case results if results.length >= min && results.length <= max => k(results.flatMap(identity))
        case _ => false
      })
    }
  }

  object Implicits {
    implicit class TreeExtension[T](tree: T)(implicit ev1: T => Tree) {
      def |(other: Tree) = Alternation(tree, other)
      def ~(other: Tree) = Concatenation(tree, other)
      def ? = Repeat(tree, 0, 1)
      def star = Repeat(tree, 0, Int.MaxValue)
      def plus = Repeat(tree, 1, Int.MaxValue)
      def doMatch(input: String) = {
        var str: String = null
        matchPrefix(tree, input.toList, {
          case result =>
            str = result.mkString
            true
        })
        str
      }
    }
    implicit def string2Tree(s: String): Tree = s.map(c => Chars(List(c)): Tree).reduce((a, b) => Concatenation(a, b))
  }

  import Implicits._

  var r: Tree = "abc" ~ "def"
  println(r doMatch "abc")
  println(r doMatch "def")
  println(r doMatch "abcdef")

  r = "abc" | "def"
  println(r doMatch "abc")
  println(r doMatch "def")
  println(r doMatch "adef")

  r = "abc".? ~ "def"
  println(r doMatch "abc")
  println(r doMatch "def")
  println(r doMatch "abcdef")

  r = "abc".plus ~ "abc"
  println(r doMatch "abcabc")
}
