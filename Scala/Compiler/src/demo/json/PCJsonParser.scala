package demo.json


class PCJsonParser extends scala.util.parsing.combinator.RegexParsers with scala.util.parsing.combinator.PackratParsers {

  private val NUMBER : Parser[Double] = """\d+(\.\d+)?""".r ^^ (_.toDouble)
  private val STRING : Parser[String] = """"(\\.|[^"])*"""".r ^^ { s => s.substring(1, s.length - 1)}
  private val BOOLEAN : Parser[Boolean] = "true|false".r ^^ (_ == "true")
  private val array : Parser[List[Any]] = "[" ~> repsep(value, ",") <~ "]"
  private val dict : Parser[Map[String, Any]] = "{" ~> repsep(STRING ~ ":" ~ value ^^ { case key ~ _ ~ _value => (key, _value)}, ",") <~ "}" ^^ (_.toMap)
  private lazy val value : Parser[Any] = BOOLEAN | NUMBER | STRING | array | dict

  def parse(input : String) = {
    val result = parseAll(value, input)
    if (result.successful) result.get else throw new Exception(result.toString())
  }
}
