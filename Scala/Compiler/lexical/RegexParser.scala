package lexical

import scala.util.parsing.combinator._
import RegexAST._
import utils.CharacterClass._

class RegexParser extends RegexParsers with PackratParsers {

  private val charParser : Parser[Seq[Char]] =
    ((("""\\.""".r ^^ { s =>
      s.charAt(1) match {
        case 't' => List('\t')
        case 'n' => List('\n')
        case 'r' => List('\r')
        case 'a' => Letters
        case 'A' => NoneLetters
        case 'd' => Digits
        case 'D' => NoneDigits
        case 'w' => LetterOrDigits
        case 'W' => NoneLetterOrDigits
        case 's' => Spaces
        case 'S' => NoneSpaces
        case c => List(c)
      }
    }) : Parser[Seq[Char]])
      | """[^\[\]\(\)\?\*\+\.\|\-]""".r ^^ (s => s.toList))

  private val rangeParser : Parser[Seq[Char]] = charParser ~ opt("-" ~! charParser) ^^ {
    case c1 ~ None => c1
    case c1 ~ Some(_ ~ c2) => {
      assert(c1.size == 1 && c2.size == 1 && c1.head < c2.head, s"Invalid Character range : $c1-$c2")
      (c1.head.toInt to c2.head.toInt).map(_.toChar)
    }
  }

  private val factorParser : Parser[Tree] = (
    "[" ~> opt("^") ~ rep1(rangeParser) <~ "]" ^^ {
      case None ~ ranges => Chars(ranges.flatMap(identity).distinct)
      case _ ~ ranges => Chars(All diff ranges.flatMap(identity).distinct)
    }
    | "(" ~> alternationParser <~ ")"
    | charParser ^^ (Chars(_))
    | "." ^^ { _ => Chars(All) })

  private val kleeneParser : Parser[Tree] = factorParser ~ opt("?" | "*" | "+") ^^ {
    case p ~ None => p
    case p ~ Some("?") => Alternation(p, Empty)
    case p ~ Some("*") => Alternation(KleenePlus(p), Empty)
    case p ~ Some("+") => KleenePlus(p)
  }

  private val concatenationParser : Parser[Tree] = rep1(kleeneParser) ^^ {
    case l => l.tail.fold(l.head) { case (a, b) => Concatenation(a, b) }
  }

  private val alternationParser : Parser[Tree] = rep1sep(concatenationParser, "|") ^^ {
    case l => l.tail.fold(l.head) { case (a, b) => Alternation(a, b) }
  }

  def parse(str : String) : Tree = parseAll(alternationParser, str).get
}