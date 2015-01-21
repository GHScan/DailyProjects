package test.demo.lua

import demo.lua._
import org.scalatest._

class LuaParserTest extends FlatSpec with Matchers {

  val parsers = List("LLBacktracking", "LALR", "LR1", "GLALR", "GLR1").map(t => new LuaParser(t))
  val pcParser = new PCLuaParser

  behavior of "LuaParser"

  it should "Work correct" in {
    val sources = List(
      """
      -- fdasjfklsdjflk a
      [[fdsafsdfjksdfjsd]]
      [=[fsdjfaklsdfkl sdf]=]

      """,

      """
       local i = 2 * 3 - j + 5
       print(i, "abcd")
      """,

      """
      print {1, 2, 3}
      print "abcdaf"
      """,

      """
      print(os.clock() + ('fadsfsd' .. "fasdjfklsdjkl"))
      """
    )


    for (source <- sources) {
      val result = pcParser.parse(source)
      for (parser <- parsers) {
        result should equal(parser.parse(source))
      }
    }
  }
}
