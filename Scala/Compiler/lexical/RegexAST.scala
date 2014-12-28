package lexical

object RegexAST {

  sealed abstract class Tree
  case object Empty extends Tree
  case class Chars[T](chars : Seq[T]) extends Tree
  case class KleeneStar(content : Tree) extends Tree
  case class Concatenation(first : Tree, second : Tree) extends Tree
  case class Alternation(first : Tree, second : Tree) extends Tree

  def transform(tree : Tree, f : Tree => Tree) : Tree = tree match {
    case Empty => f(tree)
    case Chars(_) => f(tree)
    case KleeneStar(content) => f(KleeneStar(transform(content, f)))
    case Concatenation(first, second) => f(Concatenation(transform(first, f), transform(second, f)))
    case Alternation(first, second) => f(Alternation(transform(first, f), transform(second, f)))
  }

}