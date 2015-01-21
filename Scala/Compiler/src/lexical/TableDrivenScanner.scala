package lexical


private final class TableDrivenScanner(source : ICharSource, tokenFactory : ITokenFactory, dfaEmulator : TokenizedDFAEmulator) extends IScanner {

  private var errorFound = false
  private var eofFound = false
  private val strBuilder = new StringBuilder()

  override def hasNext : Boolean = !errorFound && (source.hasNext || !eofFound)

  override def next() : IToken = {
    var state = dfaEmulator.start
    var matchState = 0
    var matchLen = 0
    var len = 0
    strBuilder.clear()

    while (state != dfaEmulator.dead && source.hasNext) {
      len += 1
      val c = source.next()
      strBuilder += c
      val category = dfaEmulator.charTable(c)
      state = dfaEmulator.transitions(state)(category.value)
      if (dfaEmulator.acceptAttrs(state) != null) {
        matchLen = len
        matchState = state
      }
    }

    for (_ <- matchLen until len) source.rollback()

    if (matchLen == 0) {
      if (source.hasNext) {
        errorFound = true
        tokenFactory.error(strBuilder.toString())
      } else {
        eofFound = true
        tokenFactory.eof()
      }
    } else {
      val attr = dfaEmulator.acceptAttrs(matchState).asInstanceOf[TokenFAStateAttribute]
      val lexeme = strBuilder.substring(0, matchLen)
      tokenFactory.create(attr.id, attr.name, attr.lexemeHandler(lexeme), lexeme)
    }
  }
}

class TableDrivenScannerBuilder extends ScannerBuilder {
  def _create(source : ICharSource, tokenFactory : ITokenFactory) : IScanner = new TableDrivenScanner(source, tokenFactory, dfaEmulator)
}