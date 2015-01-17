package benchmark.demo.lua

import org.scalatest._
import demo.lua._

class LuaParserBenchmark extends FlatSpec with Matchers {

  val parsers = List("LLBacktracking", "LALR", "LR1").map(t => new LuaParser(t))

  val source = scala.io.Source.fromFile("src/benchmark/demo/lua/scripts/Test.lua").mkString

  it should "Pass benchmark" in {

    val result = parsers.head.parse(source)

    val kTime = 20
    val kLoop = 10

    println("@ Lua parser benchmark")
    for (parser <- parsers) {
      result should equal(parser.parse(source))
      utils.Profiler.measure(s"${parser.name} [loop=$kLoop]", kTime) {
        for (_ <- 0 until kLoop) parser.parse(source)
      }
    }
  }
}
