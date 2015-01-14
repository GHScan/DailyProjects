package benchmark.demo.lua

import org.scalatest._
import demo.lua._

class LuaParserBenchmark extends FlatSpec with Matchers {

  val parsers = List("LALR", "LR1").map(t => (t, new LuaParser(t)))

  val source = scala.io.Source.fromFile("src/benchmark/demo/lua/scripts/Test.lua").mkString

  it should "Pass benchmark" in {

    val result = parsers.head._2.parse(source)

    val kTime = 20
    val kLoop = 10

    println("@ Lua parser benchmark")
    for ((t, parser) <- parsers) {
      result should equal(parser.parse(source))
      utils.Profiler.measure(s"$t [loop=$kLoop]", kTime) {
        for (_ <- 0 until kLoop) parser.parse(source)
      }
    }
  }
}
