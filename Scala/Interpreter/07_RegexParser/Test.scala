import scala.util.Random
import scala.collection.mutable
import scala.collection.immutable

object Test extends App {
  import scala.util.parsing.combinator._
  import scala.util.parsing.combinator.syntactical._

  object RegexAST {
    sealed abstract class AST
    case class CharSet(predicate : Char => Boolean) extends AST
    case class Capture(pat : AST) extends AST
    case class BackReference(index : Int) extends AST
    case class Repeat(pat : AST, min : Int, max : Int, greedy : Boolean) extends AST
    case class Concatenation(pats : List[AST]) extends AST
    case class Alternation(pats : List[AST]) extends AST
    case class BeginAnchor(pat : AST) extends AST
    case class EndAnchor(pat : AST) extends AST
  }

  class RegexParser extends RegexParsers with PackratParsers {
    import RegexAST._

    val number : Parser[Int] = """\d+""".r ^^ (_.toInt)

    val singleChar : Parser[Char] = """\\.|[^\[\]\(\)\{\}\|\.\^\$]""".r ^^ { s =>
      if (s.charAt(0) == '\\') {
        s.charAt(1) match {
          case 't' => '\t'
          case 'n' => '\n'
          case 'r' => '\r'
          case _ => s.charAt(1)
        }
      } else {
        s.charAt(0)
      }
    }

    val charRange : Parser[(Char, Char)] = singleChar ~! opt("-" ~> singleChar) ^^ {
      case c ~ None => (c, c)
      case c1 ~ Some(c2) => (c1, c2)
    }

    private def charsToSet(l : List[(Char, Char)]) : Set[Char] = {
      l.foldLeft(Set[Char]()) { case (s, (c1, c2)) => s ++ (c1 to c2) }
    }
    val charSet : Parser[CharSet] = "[" ~> opt("^") ~! rep1(charRange) <~ "]" ^^ {
      case None ~ l => { val set = charsToSet(l); CharSet(c => set(c)) }
      case _ ~ l => { val set = charsToSet(l); CharSet(c => !set(c)) }
    }

    val wildcard : Parser[CharSet] = "." ^^ { _ => CharSet(_ => true) }

    val capture : Parser[Capture] = "(" ~> alternation <~ ")" ^^ (Capture(_))

    val backReference : Parser[BackReference] = "\\" ~> number ^^ (BackReference(_))

    val factor : Parser[AST] = capture | backReference | charSet | wildcard | singleChar ^^ (c => CharSet(c2 => c == c2)) | failure("Invalid factor")

    val repeatTime : Parser[(Int, Int, Boolean)] =
      ("?" ^^ (_ => (0, 1, false))
        | "*" ^^ (_ => (0, Int.MaxValue, true))
        | "+" ^^ (_ => (1, Int.MaxValue, true))
        | "*?" ^^ (_ => (0, Int.MaxValue, false))
        | "+?" ^^ (_ => (1, Int.MaxValue, false))
        | "{" ~> number ~! opt("," ~> number) <~ "}" ^^ {
          case n1 ~ None => (n1, n1, false)
          case n1 ~ Some(n2) => (n1, n2, false)
        })

    val repeat : Parser[AST] = factor ~! opt(repeatTime) ^^ {
      case e ~ None => e
      case e ~ Some((min, max, greedy)) => Repeat(e, min, max, greedy)
    }

    val concatenation : Parser[AST] = rep1(repeat) ^^ {
      case List(e) => e
      case l => Concatenation(l)
    }

    lazy val alternation : Parser[AST] = chainl1(concatenation ^^ (List(_)), concatenation, "|" ^^ (_ => (a : List[AST], b : AST) => a ::: List(b))) ^^ {
      case List(e) => e
      case l => Alternation(l)
    }

    val regex : Parser[AST] = opt("^") ~! alternation ~! opt("$") ^^ {
      case None ~ e ~ None => e
      case None ~ e ~ _ => EndAnchor(e)
      case _ ~ e ~ None => BeginAnchor(e)
      case _ ~ e ~ _ => BeginAnchor(EndAnchor(e))
    }

    def parse(input : String) = parseAll(regex, input)
  }

  def main() {
    List(
      """\d+""",
      """\w+@\w+\.\w+""",
      """\d+(\.\d+){3}""",
      """//[^\n]*""",
      """/\*([^\*]|\*[^/])*\*/""",
      """"(\\.|[^"])*"""",
      """(\d+)\1""",
      """^\w+$""",
      "a").foreach { s =>
        println(s)
        println("\t", new RegexParser().parse(s))
      }
  }

  Utils.timeit("main", 1) {
    main()
  }
}
