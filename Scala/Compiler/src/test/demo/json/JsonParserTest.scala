package test.demo.json

import demo.json._
import org.scalatest._
import parsing.ParserFactory

class JsonParserTest extends FlatSpec with Matchers {

  val pcParser = new PCJsonParser
  val parsers = ParserFactory.get.keys.map(t => (t, new JsonParser(t)))

  behavior of "PCJsonParser"
  it should "Work correct" in {
    pcParser.parse("23.5") should equal(23.5)
    pcParser.parse( """ "abcd efg fds" """) should equal("abcd efg fds")
    pcParser.parse( """ [1, 2, 3] """) should equal(List(1, 2, 3))
    pcParser.parse( """ [1, {"a":1, "b":2}, 3] """) should equal(List(1, Map("a" -> 1, "b" -> 2), 3))
    pcParser.parse( """ [1, {"a":{"a":1, "b":"def"}, "b":[2,3]}, 3] """) should equal(List(1, Map("a" -> Map("a" -> 1, "b" -> "def"), "b" -> List(2, 3)), 3))
  }

  behavior of "Parsers"
  it should "Work correct" in {
    for ((t, parser) <- parsers) {
      parser.parse("23.5") should equal(23.5)
      parser.parse( """ "abcd efg fds" """) should equal("abcd efg fds")
      parser.parse( """ [1, 2, 3] """) should equal(List(1, 2, 3))
      parser.parse( """ [1, {"a":1, "b":2}, 3] """) should equal(List(1, Map("a" -> 1, "b" -> 2), 3))
      parser.parse( """ [1, {"a":{"a":1, "b":"def"}, "b":[2,3]}, 3] """) should equal(List(1, Map("a" -> Map("a" -> 1, "b" -> "def"), "b" -> List(2, 3)), 3))
    }
  }
}
