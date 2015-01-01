package test.lexical

import lexical.{StateAttribute, NFAVisualizer, DFAVisualizer, TokenizedNFA}
import org.scalatest.{Matchers, FlatSpec}

class DFASetOpsTest extends FlatSpec with Matchers {
  //  behavior of "NFA union"
  //  it should "correct" in {
  //    val nfa1 = TokenizedNFA.fromPattern("""for""")
  //    val nfa2 = TokenizedNFA.fromPattern("""if""")
  //    val nfa3 = TokenizedNFA.fromPattern("""\d+""")
  //    val result = (nfa1 | nfa2 | nfa3).toEmulator.toDFAEmulator.optimized.toDFA
  //    new DFAVisualizer(result).exportAsImage("dfaUnion.jpg")
  //  }

  //  behavior of "DFA reverse"
  //  it should "correct" in {
  //    val dfa = TokenizedNFA.fromPattern("ab(c|d|e)").subset
  //    val rdfa = dfa.reversed.subset
  //    new DFAVisualizer(dfa).exportAsImage("dfa.jpg")
  //    new DFAVisualizer(rdfa).exportAsImage("rdfa.jpg")
  //  }

  //    behavior of "DFA complement"
  //    it should "correct" in {
  //      val dfa = TokenizedNFA.fromPattern("ab(c|d|e)").subset
  //      val cdfa = dfa.complement.subset
  //      new DFAVisualizer(dfa).exportAsImage("dfa.jpg")
  //      new DFAVisualizer(cdfa).exportAsImage("cdfa.jpg")
  //    }

//  behavior of "DFA intersect"
//  it should "correct" in {
//    val dfa1 = TokenizedNFA.fromPattern("""\d+|for|if""", StateAttribute.Default).subset
//    val dfa2 = TokenizedNFA.fromPattern("""[a-z]+|12|34""", StateAttribute.Default).subset
//    val intersect = (dfa1 & dfa2).toEmulator.toDFAEmulator.optimized.toDFA
//    new DFAVisualizer(dfa1).exportAsImage("dfa1.jpg")
//    new DFAVisualizer(dfa2).exportAsImage("dfa2.jpg")
//    new DFAVisualizer(intersect).exportAsImage("dfai.jpg")
//  }

//  behavior of "DFA diff"
//    it should "correct" in {
//      val dfa1 = TokenizedNFA.fromPattern("""\d+|for|if""", StateAttribute.Default).subset
//      val dfa2 = TokenizedNFA.fromPattern("""[a-z]+|12""", StateAttribute.Default).subset
//      val diff = (dfa1 - dfa2).toEmulator.toDFAEmulator.optimized.toDFA
//      new DFAVisualizer(dfa1).exportAsImage("dfa1.jpg")
//      new DFAVisualizer(dfa2).exportAsImage("dfa2.jpg")
//      new DFAVisualizer(diff).exportAsImage("dfad.jpg")
//    }
}
