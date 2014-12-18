import scala.util.Random
import scala.collection.mutable
import scala.collection.immutable
import scala.util.parsing.combinator.lexical.StdLexical

object Test extends App {
  import scala.util.parsing.combinator._
  import scala.util.parsing.combinator.syntactical._

  object XMLAST {
    sealed abstract class Node
    case class TextNode(text : String) extends Node
    case class CDATANode(text : String) extends Node
    case class CommentNode(text : String) extends Node
    case class DeclarationNode(id : String, attributes : Map[String, String]) extends Node
    case class ElementNode(id : String, attributes : Map[String, String], children : List[Node]) extends Node

    implicit def nodes2Extension(nodes : List[Node]) = new {
      def trim() = {
        def process(nodes : List[Node]) : List[Node] = nodes.filter(!_.isInstanceOf[TextNode]).map {
          case n : ElementNode => ElementNode(n.id, n.attributes, process(n.children))
          case n => n
        }
        process(nodes)
      }
    }
  }

  class XMLParser extends RegexParsers with PackratParsers {
    import XMLAST._

    override protected val whiteSpace = "".r

    private def unescape(s : String) = s
      .replace("&lt;", "<")
      .replace("&gt;", ">")
      .replace("&amp;", "&")
      .replace("&apos;", "'")
      .replace("&quot;", "\"")

    val stringLiteral : Parser[String] = """".*?"""".r ^^ (s => unescape(s.substring(1, s.length - 1)))
    val identifier : Parser[String] = """[^\s>/=?!]+""".r ^^ (s => unescape(s))
    val tagWhiteSpace : Parser[String] = """\s*""".r
    def tagSymbol[T](p : Parser[T]) = tagWhiteSpace ~> p
    val tagAttribute : Parser[(String, String)] = tagSymbol(identifier) ~! tagSymbol("=") ~! tagSymbol(stringLiteral) ^^ {
      case name ~ "=" ~ value => (name, value)
    }
    val tagAttributes : Parser[Map[String, String]] = rep(tagAttribute) ^^ (_.toMap)

    val textNode : Parser[TextNode] = """[^<]+""".r ^^ (s => TextNode(unescape(s)))
    val cdataNode : Parser[CDATANode] = """<!\[CDATA\[.*?\]\]>""".r ^^ (s => CDATANode(s.substring(8, s.length - 3)))
    val commentNode : Parser[CommentNode] = """<!--.*?-->""".r ^^ (s => CommentNode(unescape(s.substring(4, s.length - 3))))
    val declarationNode : Parser[DeclarationNode] = "<?" ~> identifier ~! tagAttributes <~ tagSymbol("?>") ^^ {
      case id ~ attrs => DeclarationNode(id, attrs)
    }
    val emptyElementNode : Parser[ElementNode] = "<" ~> identifier ~ tagAttributes ~ tagSymbol("/>") ^^ {
      case name ~ attrs ~ _ => ElementNode(name, attrs, Nil)
    }
    val elementNode : Parser[ElementNode] = "<" ~> identifier ~! tagAttributes ~! tagSymbol(">") ~! rep(anyNode) ~! ("</" ~> identifier <~ ">") ^^ {
      case name ~ attrs ~ _ ~ children ~ cname => {
        if (name != cname) throw new Exception("Unclose tag: " + cname)
        else ElementNode(name, attrs, children)
      }
    }
    lazy val anyNode : Parser[Node] = cdataNode | commentNode | emptyElementNode | elementNode | textNode
    val document : Parser[List[Node]] = rep(declarationNode | textNode) ~! rep(anyNode) ^^ {
      case l1 ~ l2 => l1 ++ l2
    }

    def parse(input : String) = parseAll(document, input)
  }

  def main() {
    val s =
      """
      <!--  Edited by XMLSpy  -->
      <note>
        <to>Tove</to>
        <from>Jani</from>
        <heading>Reminder</heading>
        <body>Don't forget me this weekend!</body>
      </note>
    """
    println(new XMLParser().parse(s))
  }

  Utils.timeit("main", 1) {
    main()
  }
}
