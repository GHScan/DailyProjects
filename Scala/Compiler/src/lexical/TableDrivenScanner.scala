package lexical


private final class TableDrivenScanner(
  source : CharSource,
  tokenBuilder : TokenBuilder,
  dfaEmulator : TokenizedDFAEmulator) extends Scanner {

  private val strBuilder = new StringBuilder()

  override def hasNext : Boolean = source.hasNext

  override def next() : Token = {
    var state = dfaEmulator.start
    var matchLen = 0
    var matchState = 0
    var len = 0
    strBuilder.clear()

    while (state != dfaEmulator.dead && source.hasNext) {
      len += 1
      val c = source.next()
      strBuilder += c
      val category = dfaEmulator.charTable(c)
      state = dfaEmulator.transitions(state)(category.value)
      if (dfaEmulator.acceptAttrs(state) != None) {
        matchLen = len
        matchState = state
      }
    }

    for (_ <- len until matchLen) source.rollback()

    if (matchLen == 0) null
    else tokenBuilder.create(dfaEmulator.acceptAttrs(matchState).get.id, strBuilder.substring(0, matchLen))
  }
}

class TableDrivenScannerBuilder extends ScannerBuilder {

  override def create(source : CharSource, tokenBuilder : TokenBuilder = new FileTokenBuilder) : Scanner = new TableDrivenScanner(source, tokenBuilder, dfaEmulator)
}