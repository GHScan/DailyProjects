package benchmark.demo.json

import demo.json._
import org.scalatest._
import parsing.ParserFactory

class JsonParserBenchmark extends FlatSpec with Matchers {

  val pcParser = new PCJsonParser
  val parsers = ParserFactory.names.map(name => new JsonParser(name))

  val source = scala.io.Source.fromFile("src/benchmark/demo/json/scripts/Test.json").mkString

  behavior of "Benchmark"
  it should "Pass benchmark" in {
    val kTimes = 20
    val kLoop = 20

    val result = pcParser.parse(source)

    println("@ Json parser benchmark")
    utils.Profiler.measure(s"PC [loop=$kLoop]", kTimes) {
      for (_ <- 0 until kLoop) pcParser.parse(source)
    }
    for (parser <- parsers) {
      result should equal(parser.parse(source))
      utils.Profiler.measure(s"${parser.name} [loop=$kLoop]", kTimes) {
        for (_ <- 0 until kLoop) parser.parse(source)
      }
    }
  }
}
