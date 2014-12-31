package test.lexical

import org.scalatest._
import lexical._
import DFA2Regex._

class DFABuilderTest extends FlatSpec with Matchers {

//    behavior of "DFA of 3 Multiplier, in base10"
//    it should "correct" in {
//
//      val N = 3
//
//      val dfa = new IterativeDFABuilder()
//        .charSet('0' to '9')
//        .deadValue(-1)
//        .initialValue(0)
//        .acceptValue(0)
//        .iterateFunc { (i, c) => (i * 10 + (c - '0')) % N }
//        .result
//      new DFAVisualizer(dfa).exportAsImage("dfa.jpg")
//
//      val odfa = dfa.toEmulator.toDFAEmulator.optimized.toDFA
//      new DFAVisualizer(odfa).exportAsImage("odfa.jpg")
//
//      println(odfa.toRegexPattern)
//    }
//
//    behavior of "DFA of 3 Multiplier, in base2"
//    it should "correct" in {
//
//      val N = 3
//
//      val dfa = new IterativeDFABuilder()
//        .charSet('0' to '1')
//        .deadValue(-1)
//        .initialValue(0)
//        .acceptValue(0)
//        .iterateFunc { (i, c) => (i * 2 + (c - '0')) % N }
//        .result
//      new DFAVisualizer(dfa).exportAsImage("dfa2.jpg")
//
//      val odfa = dfa.toEmulator.toDFAEmulator.optimized.toDFA
//      new DFAVisualizer(odfa).exportAsImage("odfa2.jpg")
//
//      println(odfa.toRegexPattern)
//    }

}