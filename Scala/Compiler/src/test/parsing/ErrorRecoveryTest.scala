package test.parsing

import demo.json.JsonParser
import demo.lua.LuaParser
import org.scalatest.{FlatSpec, Matchers}

class ErrorRecoveryTest extends FlatSpec with Matchers {

//  behavior of "JsonParser"
//  it should "Report multiple errors" in {
//    for (name <- List("LL1", "LR0")) {
//      try {
//        new JsonParser(name).parse(
//          """
//          [
//          1,
//          2,
//          "fadsfsd",
//          {"a":1, "b":2, 123:324},
//          {"a":1, "b":2, []:324},
//          {"a":2, "b":3, []:324},
//          {"a":1, "b":2, "fdsafds":324},
//          {"a":5, "b":6, 324:324}
//
//          ]
//          """)
//      } catch {
//        case e : Exception =>
//          println(s"Found parse exception for $name JsonParser")
//          println(e)
//      }
//    }
//  }
//
//  behavior of "LuaParser"
//  it should "Report multiple errors" in {
//    for (name <- List("LALR")) {
//      try {
//        new LuaParser(name).parse(
//          """
//            |local a = 1  + .. 2;
//            |local b = 2   + + 3;
//            |local c = a * b (+ 2 )- 3 * 5;
//            |if a < 10 then
//            |    print(a) *
//            |elseif a < 20 then
//            |    print(a, a, a)
//            |else
//            |    repeat
//            |        local c = 3 * a
//            |    until c < 5
//            |
//            |   print());
//            |   if a == b then
//            |     3
//            |   else
//            |     a + b
//            |   end
//            |end
//            |
//
//          """.stripMargin)
//      } catch {
//        case e : Exception =>
//          println(s"Found parse exception for $name LuaParser")
//          println(e)
//      }
//    }
//  }
}
