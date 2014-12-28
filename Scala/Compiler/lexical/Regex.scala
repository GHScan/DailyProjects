package lexical

trait Regex {
  def matchPrefix(s : String) : String
  def isMatch(s : String) : Boolean = matchPrefix(s) == s
}

object Regex {

  def patternEquals(pattern1 : String, pattern2 : String) : Boolean = {
    val dfa1 = TokenizedDFAEmulator(TokenizedNFAEmulator(TokenizedNFA.fromRegex(pattern1, 0))).optimize()
    val dfa2 = TokenizedDFAEmulator(TokenizedNFAEmulator(TokenizedNFA.fromRegex(pattern2, 0))).optimize()

    TokenizedNFAEmulator(dfa1.toDFA()) == TokenizedNFAEmulator(dfa2.toDFA())
  }

}

class NFARegex(pattern : String) extends Regex {
  val nfa = TokenizedNFAEmulator(TokenizedNFA.fromRegex(pattern))

  def matchPrefix(s : String) : String = {
    val source = new StringCharSource(s)

    val result = new StringBuilder()
    var matchLen = 0
    var stateSet = nfa.closure(nfa.start)
    var len = 0
    while (!stateSet.isEmpty && source.hasNext) {
      len += 1

      val c = source.next()
      val category = nfa.charMap(c)
      stateSet = nfa.move(stateSet, category)
      result += c

      if (!(stateSet & nfa.acceptSet).isEmpty) matchLen = len
    }

    for (_ <- matchLen until len) source.rollback()
    if (matchLen > 0) result.toString.substring(0, matchLen) else ""
  }
}

class DFARegex(pattern : String) extends Regex {
  val dfa = TokenizedDFAEmulator(TokenizedNFAEmulator(TokenizedNFA.fromRegex(pattern))).optimize()

  def matchPrefix(s : String) : String = {
    val source = new StringCharSource(s)

    val result = new StringBuilder()
    var matchLen = 0
    var state = dfa.start
    var len = 0
    while (state != dfa.dead && source.hasNext) {
      len += 1

      val c = source.next()
      val category = dfa.charMap(c)
      state = dfa.transitions(state)(category.value)
      result += c

      if (dfa.acceptAttrs(state) != None) matchLen = len
    }

    for (_ <- matchLen until len) source.rollback()
    if (matchLen > 0) result.toString.substring(0, matchLen) else ""
  }
}