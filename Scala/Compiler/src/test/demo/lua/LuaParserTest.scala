package test.demo.lua

import org.scalatest._
import demo.lua._

class LuaParserTest extends FlatSpec with Matchers {

  val parsers = List("SLR", "LALR", "LR1").map(t => (t, new LuaParser(t)))

  behavior of "LuaParser"
  it should "Pass benchmark" in {

    val source = scala.io.Source.fromFile("src/test/demo/lua/scripts/Test.lua").mkString

    val result = parsers.head._2.parse(source)

    val kTime = 20
    val kLoop = 10
    for ((t, parser) <- parsers) {
      result should equal(parser.parse(source))
      utils.Profiler.measure(s"$t [loop=$kLoop]", kTime) {
        for (_ <- 0 until kLoop) parser.parse(source)
      }
    }
  }
}
