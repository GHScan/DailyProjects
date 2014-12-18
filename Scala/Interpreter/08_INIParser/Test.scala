import scala.util.Random
import scala.collection.mutable
import scala.collection.immutable
import scala.util.parsing.combinator.lexical.StdLexical

object Test extends App {
  import scala.util.parsing.combinator._
  import scala.util.parsing.combinator.syntactical._

  class INIParser extends RegexParsers with PackratParsers {
    val name : Parser[String] = """\w+""".r
    val lineContent : Parser[String] = """[^\r\n]*(\r|\n)""".r

    override protected val whiteSpace = """(\s|;[^\r\n]*(\r|\n))+""".r

    val property : Parser[(String, String)] = name ~! "=" ~! lineContent ^^ { case n ~ "=" ~ v => (n, v) }
    val section : Parser[(String, Map[String, String])] = ("[" ~> name <~ "]") ~! rep(property) ^^ {
      case name ~ l => (name, Map() ++ l)
    }
    val file : Parser[Map[String, Map[String, String]]] = rep(section) ^^ {
      case l => l.foldLeft[Map[String, Map[String, String]]](Map()) {
        case (m, (sname, kvs)) => m + ((sname, kvs))
      }
    }

    def parse(input : String) = parseAll(file, input)
  }

  def main() {
    val s =
      """
      ; last modified 1 April 2001 by John Doe
      [owner]
      name=John Doe
      organization=Acme Widgets Inc.
       
      [database]
      ; use IP address in case network name resolution is not working
      server=192.0.2.62     
      port=143
      file="payroll.dat"
      """
    println(new INIParser().parse(s))
  }

  Utils.timeit("main", 1) {
    main()
  }
}
