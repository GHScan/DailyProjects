package lexical

import scala.collection.immutable
import utils.Characters._

object RegexAST {

  sealed abstract class Tree

  case object Empty extends Tree

  case class Chars[T](chars : Seq[T]) extends Tree

  case class KleeneStar(content : Tree) extends Tree

  case class Concatenation(first : Tree, second : Tree) extends Tree

  case class Alternation(first : Tree, second : Tree) extends Tree

  object KleenePlus {
    def unapply(tree : Tree) : Option[Tree] = tree match {
      case Concatenation(KleeneStar(k), c) if k == c => Some(k)
      case Concatenation(c, KleeneStar(k)) if k == c => Some(k)
      case _ => None
    }
  }

  object QuestionMark {
    def unapply(tree : Tree) : Option[Tree] = tree match {
      case Alternation(Empty, a) if a != Empty => Some(a)
      case Alternation(a, Empty) => Some(a)
      case _ => None
    }
  }

  private lazy val characterClasses : List[(immutable.BitSet, String)] = List(
    (NoneWhitespaces, "\\S"),
    (NoneDigits, "\\D"),
    (NoneLetters, "\\A"),
    (NoneLetterOrDigits, "\\W"),
    (LetterOrDigits, "\\w"),
    (Letters, "\\a"),
    (Digits, "\\d"),
    (Whitespaces, "\\s")).map(p => (immutable.BitSet(p._1.map(_.toInt) : _*), p._2))


  implicit class RegexExtension(tree : Tree) {

    def toPattern : String = {

      def escapeChar(c : Char) : String = c match {
        case '\t' => "\\t"
        case '\n' => "\\n"
        case '\r' => "\\r"
        case _ => c.toString
      }

      def charsToPattern(chars : Seq[Char]) : String = {
        var result = ""
        var charSet = immutable.BitSet(chars.map(_.toInt) : _*)
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

      def iterate(tree : Tree, parentPriority : Int) : String = {
        val str = tree match {
          case Chars(chars : Seq[_]) if chars.head.isInstanceOf[Char] => charsToPattern(chars.asInstanceOf[Seq[Char]])
          case KleeneStar(content) => s"${iterate(content, tree.priority)}*"
          case KleenePlus(k) => s"${iterate(k, KleeneStar(k).priority)}+"
          case Concatenation(first, second) => s"${iterate(first, tree.priority)}${iterate(second, tree.priority)}"
          case QuestionMark(q) => s"${iterate(q, KleeneStar(q).priority)}?"
          case Alternation(first, second) => s"${iterate(first, tree.priority)}|${iterate(second, tree.priority)}"
          case _ =>
            sys.error("Found invalid regex: " + tree)
            ""
        }

        if (tree.priority < parentPriority) s"($str)" else str
      }

      iterate(tree, 0)
    }

    def priority : Int = tree match {
      case null => 5
      case Empty => 4
      case Chars(_) => 3
      case KleeneStar(_) => 2
      case Concatenation(_, _) => 1
      case Alternation(_, _) => 0
    }


    def simplified : Tree = {
      def iterate(tree : Tree) : Tree = tree match {
        case Empty => tree
        case Chars(chars) => tree
        case KleeneStar(content) => iterate(content).kleeneStar
        case Concatenation(first, second) => iterate(first) & iterate(second)
        case Alternation(first, second) => iterate(first) | iterate(second)
      }

      iterate(tree)
    }


    def transform(f : Tree => Tree) : Tree = {
      def iterate(tree : Tree) : Tree = tree match {
        case Empty => f(tree)
        case Chars(_) => f(tree)
        case KleeneStar(content) => f(KleeneStar(iterate(content)))
        case Concatenation(first, second) => f(Concatenation(iterate(first), iterate(second)))
        case Alternation(first, second) => f(Alternation(iterate(first), iterate(second)))
      }
      iterate(tree)
    }

    def kleeneStar : Tree = tree match {
      case null => Empty
      case Empty => Empty
      case KleeneStar(_) => tree
      case QuestionMark(a) => a.kleeneStar
      case _ => KleeneStar(tree)
    }

    def &(other : Tree) : Tree = (tree, other) match {
      case (null, _) => null
      case (_, null) => null
      case (Empty, _) => other
      case (_, Empty) => tree
      case (KleenePlus(k), _) if k == other => tree
      case (_, KleenePlus(k)) if k == tree => other
      case (KleeneStar(content), QuestionMark(q)) if content == q => tree
      case (QuestionMark(q), KleeneStar(content)) if content == q => other
      case (Concatenation(c1, c2), _) => c1 & (c2 & other)
      case (_, Concatenation(c1, c2)) => tree & c1 match {
        case Concatenation(cc1, cc2) => Concatenation(cc1, cc2 & c2)
        case r => r & c2
      }
      case _ => Concatenation(tree, other)
    }

    def |(other : Tree) : Tree =
      if (tree.priority < other.priority) other | tree
      else if (tree == other) tree
      else (tree, other) match {
        case (null, _) => other
        case (Empty, KleeneStar(_)) => other
        case (Empty, KleenePlus(k)) => KleeneStar(k)
        case (Chars(chars1), Chars(chars2)) => Chars((chars1 ++ chars2).distinct)
        case (Chars(_), KleeneStar(k)) if tree == k => other
        case (KleeneStar(k), _) if k == other => tree
        case (Concatenation(c1, c2), Concatenation(c3, c4)) if c1 == c3 => c1 & (c2 | c4)
        case (Concatenation(c1, c2), Concatenation(c3, c4)) if c2 == c4 => (c1 | c3) & c2
        case (Alternation(a1, a2), Alternation(_, _)) => a1 | (a2 | other)
        case (_, Alternation(a1, a2)) => tree | a1 match {
          case Alternation(aa1, aa2) => Alternation(aa1, aa2 | a2)
          case r => r | a2
        }
        case (_, _) => Alternation(tree, other)
      }
  }

}

