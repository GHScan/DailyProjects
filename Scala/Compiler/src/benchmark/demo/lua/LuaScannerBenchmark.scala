package benchmark.demo.lua

import demo.lua._
import org.scalatest._

class LuaScannerBenchmark extends FlatSpec with Matchers {

  val source = scala.io.Source.fromFile("src/benchmark/demo/lua/scripts/Test.lua").mkString

  it should "Pass benchmark" in {
    val kTime = 20
    val kLoop = 10

    println("@ Lua Scanner benchmark")
    utils.Profiler.measure(s"Scanner [loop=$kLoop]", kTime) {
      for (_ <- 0 until kLoop) {
        val scanner = LuaScanner.create(source)
        while (scanner.hasNext) scanner.next()
      }
    }
  }
}
