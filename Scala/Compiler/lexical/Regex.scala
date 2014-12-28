package lexical

trait Regex {
  def matchPrefix(s : String) : String
  def isMatch(s : String) : Boolean = matchPrefix(s) == s
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