import scala.util.Random
import scala.collection.mutable
import scala.collection.immutable

object Test extends App {
  import scala.util.parsing.combinator._
  import scala.util.parsing.combinator.syntactical._

  class JsonParser extends StandardTokenParsers with PackratParsers {
    lexical.delimiters += (",", ":", "[", "]", "{", "}")

    private val array : Parser[List[Any]] = "[" ~> chainl1(value ^^ (List(_)), value, "," ^^ (_ => (a : List[Any], b : Any) => a ::: List(b))) <~ "]"
    private val keyValue : Parser[(String, Any)] = stringLit ~! ":" ~! value ^^ { case (a ~ ":" ~ b) => (a, b) }
    private val dict : Parser[Map[String, Any]] = "{" ~> chainl1(keyValue ^^ (Map(_)), keyValue, "," ^^ (_ => (a : Map[String, Any], b : (String, Any)) => a + b)) <~ "}"
    private lazy val value : Parser[Any] = numericLit ^^ (_.toInt) | stringLit | array | dict
    private def parseAll[T](p : Parser[T], input : String) = phrase(p)(new lexical.Scanner(input))
    def parse(input : String) = parseAll(value, input)

  }

  def main() {
    List("""234""",
      """ "afsdjk" """,
      """[3,2,"fads",4]  """,
      """{"a":1,"b":2,"c":12}  """,
      """{"a":1,"b":2,"c":[1,2,{"d":4,"e":5}]}  """).foreach { s =>
        println(new JsonParser().parse(s))
      }
  }

  Utils.timeit("main", 1) {
    main()
  }
}
