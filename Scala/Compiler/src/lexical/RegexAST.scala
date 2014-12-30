package lexical

import scala.collection.immutable
import utils.Characters._

object RegexAST {

  sealed abstract class Tree

  case object Empty extends Tree

  case class Chars[T](chars: Seq[T]) extends Tree

  case class KleeneStar(content: Tree) extends Tree

  case class Concatenation(first: Tree, second: Tree) extends Tree

  case class Alternation(first: Tree, second: Tree) extends Tree

  object KleenePlus {
    def unapply(tree: Tree): Option[Tree] = tree match {
      case Concatenation(KleeneStar(k), c) if k == c => Some(k)
      case Concatenation(c, KleeneStar(k)) if k == c => Some(k)
      case _ => None
    }
  }

  object QuestionMark {
    def unapply(tree: Tree): Option[Tree] = tree match {
      case Alternation(Empty, a) if a != Empty => Some(a)
      case Alternation(a, Empty) => Some(a)
      case _ => None
    }
  }

  def transform(tree: Tree, f: Tree => Tree): Tree = tree match {
    case Empty => f(tree)
    case Chars(_) => f(tree)
    case KleeneStar(content) => f(KleeneStar(transform(content, f)))
    case Concatenation(first, second) => f(Concatenation(transform(first, f), transform(second, f)))
    case Alternation(first, second) => f(Alternation(transform(first, f), transform(second, f)))
  }

  def getPriority(tree: Tree): Int = tree match {
    case null => 5
    case Empty => 4
    case Chars(_) => 3
    case KleeneStar(_) => 2
    case Concatenation(_, _) => 1
    case Alternation(_, _) => 0
  }

  def simplify(tree: Tree): Tree = tree match {
    case Empty => tree
    case Chars(chars) => tree
    case KleeneStar(content) => Provider.kleeneStar(simplify(content))
    case Concatenation(first, second) => Provider.concatenation(simplify(first), simplify(second))
    case Alternation(first, second) => Provider.alternation(simplify(first), simplify(second))
  }

  object Provider {

    def none: Tree = null

    def empty: Tree = Empty

    def chars(chars: Seq[Char]): Tree = Chars(chars)

    def kleeneStar(content: Tree): Tree = content match {
      case null => empty
      case Empty => empty
      case KleeneStar(_) => content
      case QuestionMark(a) => kleeneStar(a)
      case _ => KleeneStar(content)
    }

    def concatenation(first: Tree, second: Tree): Tree = (first, second) match {
      case (null, _) => null
      case (_, null) => null
      case (Empty, _) => second
      case (_, Empty) => first
      case (KleenePlus(k), _) if k == second => first
      case (_, KleenePlus(k)) if k == first => second
      case (KleeneStar(content), QuestionMark(q)) if content == q => first
      case (QuestionMark(q), KleeneStar(content)) if content == q => second
      case (Concatenation(c1, c2), _) => concatenation(c1, concatenation(c2, second))
      case (_, Concatenation(c1, c2)) => concatenation(first, c1) match {
        case Concatenation(cc1, cc2) => Concatenation(cc1, concatenation(cc2, c2))
        case r => concatenation(r, c2)
      }
      case _ => Concatenation(first, second)
    }

    def alternation(first: Tree, second: Tree): Tree =
      if (getPriority(first) < getPriority(second)) alternation(second, first)
      else if (first == second) first
      else (first, second) match {
        case (null, _) => second
        case (Empty, KleeneStar(_)) => second
        case (Empty, KleenePlus(k)) => KleeneStar(k)
        case (Chars(chars1), Chars(chars2)) => Chars((chars1 ++ chars2).distinct)
        case (Chars(_), KleeneStar(k)) if first == k => second
        case (KleeneStar(k), _) if k == second => first
        case (Concatenation(c1, c2), Concatenation(c3, c4)) if c1 == c3 => concatenation(c1, alternation(c2, c4))
        case (Concatenation(c1, c2), Concatenation(c3, c4)) if c2 == c4 => concatenation(alternation(c1, c3), c2)
        case (Alternation(a1, a2), Alternation(_, _)) => alternation(a1, alternation(a2, second))
        case (_, Alternation(a1, a2)) => alternation(first, a1) match {
          case Alternation(aa1, aa2) => Alternation(aa1, alternation(aa2, a2))
          case r => alternation(r, a2)
        }
        case (_, _) => Alternation(first, second)
      }
  }

  private lazy val characterClasses: List[(immutable.BitSet, String)] = List(
    (immutable.BitSet(NoneWhitespaces.map(_.toInt): _*), "\\S"),
    (immutable.BitSet(NoneDigits.map(_.toInt): _*), "\\D"),
    (immutable.BitSet(NoneLetters.map(_.toInt): _*), "\\A"),
    (immutable.BitSet(NoneLetterOrDigits.map(_.toInt): _*), "\\W"),
    (immutable.BitSet(LetterOrDigits.map(_.toInt): _*), "\\w"),
    (immutable.BitSet(Letters.map(_.toInt): _*), "\\a"),
    (immutable.BitSet(Digits.map(_.toInt): _*), "\\d"),
    (immutable.BitSet(Whitespaces.map(_.toInt): _*), "\\s"))

  def toPattern(tree: Tree): String = {

    def escapeChar(c: Char): String = c match {
      case '\t' => "\\t"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case _ => c.toString
    }

    def charsToPattern(chars: Seq[Char]): String = {
      var result = ""
      var charSet = immutable.BitSet(chars.map(_.toInt): _*)
      for (cClass <- characterClasses) {
        if ((cClass._1 & charSet) == cClass._1) {
          charSet --= cClass._1
          result += cClass._2
        }
      }

      val groups = utils.Func.splitToContinuesSegments(charSet.map(_.toChar).toList)
      result += groups.map {
        case List(x) => escapeChar(x)
        case List(x, y) => s"${escapeChar(x)}${escapeChar(y)}"
        case List(x, y, z) => s"${escapeChar(x)}${escapeChar(y)}${escapeChar(z)}"
        case l => s"${escapeChar(l.head)}-${escapeChar(l.last)}"
      }.mkString

      result.length match {
        case 1 => result
        case 2 if result.charAt(0) == '\\' => result
        case _ => s"[$result]"
      }
    }

    def iterate(tree: Tree, parentPriority: Int): String = {
      val str = tree match {
        case Chars(chars: Seq[_]) if chars.head.isInstanceOf[Char] => charsToPattern(chars.asInstanceOf[Seq[Char]])
        case KleeneStar(content) => s"${iterate(content, getPriority(tree))}*"
        case KleenePlus(k) => s"${iterate(k, getPriority(KleeneStar(k)))}+"
        case Concatenation(first, second) => s"${iterate(first, getPriority(tree))}${iterate(second, getPriority(tree))}"
        case QuestionMark(q) => s"${iterate(q, getPriority(KleeneStar(q)))}?"
        case Alternation(first, second) => s"${iterate(first, getPriority(tree))}|${iterate(second, getPriority(tree))}"
        case _ =>
          sys.error("Found invalid regex: " + tree)
          ""
      }

      if (getPriority(tree) < parentPriority) s"($str)" else str
    }

    iterate(tree, 0)
  }
}