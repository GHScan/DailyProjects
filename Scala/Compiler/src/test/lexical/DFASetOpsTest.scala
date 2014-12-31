package test.lexical

import lexical.{NFAVisualizer, DFAVisualizer, TokenizedNFA}
import org.scalatest.{Matchers, FlatSpec}

class DFASetOpsTest extends FlatSpec with Matchers {
//  behavior of "NFA Union"
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
//    new DFAVisualizer(dfa).exportAsImage("nfa.jpg")
//    new DFAVisualizer(rdfa).exportAsImage("rnfa.jpg")
//  }
}
