object ParserCombinator {
  abstract class Parsers {
    type Elem

    abstract class Location
    abstract class Reader {
      def location : Location
      def elem : Elem
      def isEof : Boolean
    }

    sealed abstract class ParseResult[+T] {
      def flatMap[U](f : (T, Reader) => ParseResult[U]) : ParseResult[U]
    }
    case class Success[T](value : T, reader : Reader) extends ParseResult[T] {
      override def toString = s"Success : $value"
      override def flatMap[U](f : (T, Reader) => ParseResult[U]) = f(value, reader)
    }
    case class Failure(message : String, reader : Reader) extends ParseResult[Nothing] {
      override def toString = s"Failure: $message (${reader.location}, ${reader.elem})"
      override def flatMap[U](f : (Nothing, Reader) => ParseResult[U]) = this
    }
    case class Error(message : String, reader : Reader) extends ParseResult[Nothing] {
      override def toString = s"Error: $message (${reader.location}, ${reader.elem})"
      override def flatMap[U](f : (Nothing, Reader) => ParseResult[U]) = this
    }

    abstract class Parser[+T] extends (Reader => ParseResult[T]) {
      outer =>

      def concatenate[U, P](b : => Parser[U], f : (T, U) => P) = new Parser[P] {
        def apply(reader : Reader) = outer(reader).flatMap { (value, reader) =>
          b(reader).flatMap { (value2, reader2) => Success(f(value, value2), reader2) }
        }
      }
      def concatenate[U](b : => Parser[U]) = concatenate[U, (T, U)](b, (_, _))
      def alternative[U >: T](b : => Parser[U]) = new Parser[U] {
        def apply(reader : Reader) = {
          outer(reader) match {
            case v : Success[T] => v
            case v : Failure => b(reader)
            case v : Error => v
          }
        }
      }

      def ~[U](b : => Parser[U]) = concatenate(b)
      def ~![U](b : => Parser[U]) = concatenate(b.alternative(error("~!")))
      def ~>[U](b : => Parser[U]) = concatenate(b, (a : T, b : U) => b)
      def <~[U](b : => Parser[U]) = concatenate(b, (a : T, b : U) => a)
      def |[U >: T](b : => Parser[U]) = alternative(b)
      def ^^[U](f : T => U) = new Parser[U] {
        def apply(reader : Reader) = outer(reader).flatMap { (value, reader) => Success(f(value), reader) }
      }
    }

    object ~ {
      def unapply[T, U](a : (T, U)) : Option[(T, U)] = a match {
        case (i, j) => Some(i, j)
      }
    }

    def success[T](value : T) = new Parser[T] {
      def apply(reader : Reader) = Success(value, reader)
    }
    def failure(message : String) = new Parser[Nothing] {
      def apply(reader : Reader) = Failure(message, reader)
    }
    def error(message : String) = new Parser[Nothing] {
      def apply(reader : Reader) = Error(message, reader)
    }
    def opt[T](a : => Parser[T]) : Parser[Option[T]] = a ^^ (Some(_) : Option[T]) | success(None)
    def rep[T](a : => Parser[T]) : Parser[List[T]] = a ~ rep(a) ^^ { case x ~ xs => x :: xs } | success(Nil)
    def rep1[T](a : => Parser[T]) : Parser[List[T]] = a ~ rep(a) ^^ { case x ~ xs => x :: xs }
    def repN[T](a : => Parser[T], n : Int) : Parser[List[T]] = {
      n match {
        case 0 => success(List())
        case _ => a ~ repN(a, n - 1) ^^ { case x ~ xs => x :: xs }
      }
    }
    def chainl1[T, U](first : => Parser[T], a : => Parser[U], sep : => Parser[(T, U) => T]) : Parser[T] = first ~ rep(sep ~! a) ^^ {
      case x ~ xs => xs.foldLeft(x) { case (x, op ~ v) => op(x, v) }
    }
    def chainl1[T](a : => Parser[T], sep : => Parser[(T, T) => T]) : Parser[T] = chainl1[T, T](a, a, sep)
    def rep1sep[T](a : => Parser[T], sep : => Parser[_]) : Parser[List[T]] = chainl1[List[T], T](a ^^ (List(_)), a, sep ^^ (_ => (xs, x) => x :: xs))
    def repsep[T](a : => Parser[T], sep : => Parser[_]) : Parser[List[T]] = rep1sep(a, sep) | success(Nil)
    def log[T](a : => Parser[T])(name : String) = new Parser[T] {
      def apply(reader : Reader) = { println("log: " + name); a(reader) }
    }
    def phrase[T](a : => Parser[T]) = new Parser[T] {
      def apply(reader : Reader) = a(reader).flatMap { (value, reader) =>
        if (reader.isEof) Success(value, reader) else Failure("Input is longer than a phrase", reader)
      }
    }
  }

  class RegexParsers extends Parsers {
    type Elem = Char

    case class StringLocation(line : Int, column : Int) extends Location
    case class StringReader(input : String, loc : StringLocation) extends Reader {
      def elem = input.charAt(0)
      def location = loc
      def isEof = input.isEmpty
      def advance(step : Int) : StringReader = {
        def iterate(i : Int, line : Int, column : Int) : StringLocation = {
          if (i == step) StringLocation(line, column)
          else if (input.charAt(i) == '\n') iterate(i + 1, line + 1, 1)
          else iterate(i + 1, line, column + 1)
        }
        val StringLocation(line, column) = loc
        new StringReader(input.substring(step), iterate(0, line, column))
      }
    }

    def buildParserFromRegex(reg : scala.util.matching.Regex) = new Parser[String] {
      def apply(reader : Reader) = reader match {
        case r@StringReader(input, loc) => {
          reg.findPrefixOf(input) match {
            case Some(s) => Success(s, r.advance(s.length))
            case _ => Failure(s"Exprected $reg", reader)
          }
        }
      }
    }
    def buildParserFromString(s : String) = new Parser[String] {
      def apply(reader : Reader) = reader match {
        case r@StringReader(input, loc) if input.startsWith(s) => Success(s, r.advance(s.length))
        case _ => Failure(s"Expected $s", reader)
      }
    }

    implicit def string2Parser(s : String) = opt(whilteSpaceParser) ~> buildParserFromString(s)
    implicit def regex2Parser(reg : scala.util.matching.Regex) = opt(whilteSpaceParser) ~> buildParserFromRegex(reg)

    def writeSpace : scala.util.matching.Regex = """\s+""".r
    def whilteSpaceParser : Parser[String] = buildParserFromRegex(writeSpace)

    def parseAll[T](p : Parser[T], input : String) = phrase(p <~ opt(whilteSpaceParser))(new StringReader(input, StringLocation(1, 1)))
  }

}
