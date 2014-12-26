package lexical

object RegexAST {
  sealed abstract class Tree
  case object Empty extends Tree
  case class Chars(chars : Seq[Char]) extends Tree
  case class KleenePlus(content : Tree) extends Tree
  case class Concatenation(first : Tree, second : Tree) extends Tree
  case class Alternation(first : Tree, second : Tree) extends Tree
}