package lexical

trait Regex {

  def matchPrefix(s : String) : String

  def isMatch(s : String) : Boolean = matchPrefix(s) == s
}

object Regex {

  def patternEquals(pattern1 : String, pattern2 : String) : Boolean = {
    TokenizedNFA.fromPattern(pattern1, 0, "").toEmulator.toDFAEmulator.optimized.toDFA.toEmulator ==
      TokenizedNFA.fromPattern(pattern2, 0, "").toEmulator.toDFAEmulator.optimized.toDFA.toEmulator
  }

}

final class NFARegex(pattern : String) extends Regex {
  val nfa = TokenizedNFA.fromPattern(pattern).toEmulator

  def matchPrefix(s : String) : String = {
    val source = new StringCharSource(s)

    val result = new StringBuilder()
    var matchLen = 0
    var stateSet = nfa.closure(nfa.start)
    var len = 0
    while (stateSet.nonEmpty && source.hasNext) {
      len += 1

      val c = source.next()
      val category = nfa.charTable(c)
      stateSet = nfa.move(stateSet, category)
      result += c

      if ((stateSet & nfa.acceptSet).nonEmpty) matchLen = len
    }

    for (_ <- matchLen until len) source.rollback()
    if (matchLen > 0) result.mkString.substring(0, matchLen) else ""
  }
}

final class DFARegex(pattern : String) extends Regex {
  val dfa = TokenizedNFA.fromPattern(pattern).toEmulator.toDFAEmulator.optimized

  def matchPrefix(s : String) : String = {
    val source = new StringCharSource(s)

    val result = new StringBuilder()
    var matchLen = 0
    var state = dfa.start
    var len = 0
    while (state != dfa.dead && source.hasNext) {
      len += 1

      val c = source.next()
      val category = dfa.charTable(c)
      state = dfa.transitions(state)(category.value)
      result += c

      if (dfa.acceptAttrs(state) != None) matchLen = len
    }

    for (_ <- matchLen until len) source.rollback()
    if (matchLen > 0) result.mkString.substring(0, matchLen) else ""
  }
}