package lexical


private final class TableDrivenScanner(source : ICharSource, tokenFactory : ITokenFactory, dfaEmulator : TokenizedDFAEmulator) extends IScanner {

  private val strBuilder = new StringBuilder()

  override def hasNext : Boolean = source.hasNext

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
      if (source.hasNext) tokenFactory.error(strBuilder.toString())
      else tokenFactory.eof()
    } else {
      val attr = dfaEmulator.acceptAttrs(matchState).asInstanceOf[TokenFAStateAttribute]
      val lexeme = strBuilder.substring(0, matchLen)
      val value = attr.lexemeHandler(lexeme)
      if (value == null) tokenFactory.create(attr.id, attr.name, lexeme)
      else tokenFactory.createExt(attr.id, attr.name, lexeme, value)
    }
  }
}

class TableDrivenScannerBuilder extends ScannerBuilder {

  override def create(source : ICharSource, tokenFactory : ITokenFactory = new FileTokenFactory) : IScanner = new TableDrivenScanner(source, tokenFactory, dfaEmulator)
}