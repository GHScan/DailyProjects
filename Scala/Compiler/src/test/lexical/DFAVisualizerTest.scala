package test.lexical

import org.scalatest._

class DFAVisualizerTest extends FlatSpec with Matchers {
  behavior of "basic visualizer"
  it should "" in {
//    val nfa = TokenizedNFA.fromPattern( """([0369]|([147]|[258][0369]*[258])([147][0369]*[258]|[0369])*[258]|([258]|[147][0369]*[147])([258][0369]*[147]|[0369])*[147])+""")
//    // val nfa = TokenizedNFA.fromPattern("""[0369]*(([147][0369]*|[258][0369]*[258][0369]*)([147][0369]*[258][0369]*)*([258][0369]*|[147][0369]*[147][0369]*)|[258][0369]*[147][0369]*)*""")
//    // val nfa = TokenizedNFA.fromPattern( """if|else|for|struct|\d+|[A-Z]+""")
//    // val nfa = TokenizedNFA.fromPattern("""0|1|2|3|4|5|6|7|8|9|10|11|12|13|14|15""")
//    // new NFAVisualizer(nfa).exportAsImage("nfa.jpg")
//
//    val dfaEmulator = nfa.toDFAEmulator
//    new DFAVisualizer(dfaEmulator.toDFA).exportAsImage("dfa.jpg")
//
//    val dfaEmulator2 = dfaEmulator.minimized
//    new DFAVisualizer(dfaEmulator2.toDFA).exportAsImage("odfa.jpg")
//
//    val dfaEmulator3 = dfaEmulator2.charTableCompacted
//    new DFAVisualizer(dfaEmulator3.toDFA).exportAsImage("odfa2.jpg")
  }
}