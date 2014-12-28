package lexical.test

import org.scalatest._
import lexical.{ TokenizedNFA, TokenizedNFAEmulator }
import lexical.TokenizedDFAEmulator
import lexical.NFAVisualizer
import lexical.DFAVisualizer

class DFAVisualizerTest extends FlatSpec with Matchers {
  behavior of "basic visualizer"
  it should "" in {
    val nfa = TokenizedNFA.fromRegex("""\d+|\w+""")
    new NFAVisualizer(nfa).exportAsImage("nfa.jpg")
    val dfa = TokenizedDFAEmulator(TokenizedNFAEmulator(nfa)).toDFA()
    new NFAVisualizer(dfa).exportAsImage("dfa_1.jpg")
    new DFAVisualizer(dfa).exportAsImage("dfa_2.jpg")
  }
}