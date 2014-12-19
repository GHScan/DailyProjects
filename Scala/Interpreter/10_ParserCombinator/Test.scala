import scala.collection.mutable
import scala.collection.immutable

object Test extends App {
  import ParserCombinator._

  //----------------------------------------------------------------------------
  class JsonParser extends RegexParsers {
    private val numberLiteral : Parser[Int] = """\d+""".r ^^ (_.toInt)
    private val stringLiteral : Parser[String] = """".*?"""".r
    private val keyValuePair : Parser[(String, Any)] = stringLiteral ~ ":" ~ value ^^ { case k ~ ":" ~ v => (k, v) }
    private val dict : Parser[Map[String, Any]] = "{" ~> repsep(keyValuePair, ",") <~ "}" ^^ (_.toMap)
    private val array : Parser[List[Any]] = "[" ~> repsep(value, ",") <~ "]"
    private val value : Parser[Any] = array | dict | numberLiteral | stringLiteral
    def parse(input : String) = parseAll(value, input)
  }

  def testJsonParser() {
    List("""234""",
      """ "afsdjk" """,
      """[3,2,"fads",4]  """,
      """{"a":1,"b":2,"c":12}  """,
      """{"a":1,"b":2,"c":[1,2,{"d":4,"e":5}]}  """).foreach { s =>
        println(new JsonParser().parse(s))
      }
  }
  //----------------------------------------------------------------------------
  class ProtoParser extends RegexParsers {
    object AST {
      sealed abstract class Type
      case class PrimitiveType(name : String) extends Type
      case class ArrayType(elemType : Type, dimension : Int) extends Type
      case class Field(name : String, _type : Type)
      case class Class(name : String, fields : List[Field]) extends Type
    }
    import AST._

    private val ident : Parser[String] = """\w+""".r
    private val number : Parser[Int] = """\d+""".r ^^ (_.toInt)
    private val typeDecl : Parser[Type] = ident ~ rep("[" ~> number <~ "]") ^^ {
      case id ~ l =>
        l.foldRight(PrimitiveType(id) : Type) { case (n, t) => ArrayType(t, n) }
    }
    private val fieldDecl : Parser[Field] = typeDecl ~! ident ~! ";" ^^ { case t ~ id ~ _ => Field(id, t) }
    private val classDecl : Parser[Class] = "class" ~! ident ~! "{" ~! rep(fieldDecl) ~! "}" ^^ {
      case _ ~ id ~ _ ~ fields ~ _ => Class(id, fields)
    }
    private val program : Parser[List[Class]] = rep(classDecl)
    def parse(input : String) = parseAll(program, input)
  }

  def testProtoParser() {
    val s = """
      class Person {
        char[16] name;
        int age;
        string[4] addresses; 
      }
      class Street {
        Person[4][64] persns;
      }
      """
    println(new ProtoParser().parse(s))
  }

  //----------------------------------------------------------------------------
  def main() {
    testJsonParser()
    testProtoParser()
  }

  Utils.timeit("main", 1) {
    main()
  }
}
