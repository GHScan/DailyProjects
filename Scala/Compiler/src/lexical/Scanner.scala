package lexical

class Token(id : String, lexeme : String) {
  override def toString = if (id == lexeme) s"Token($id)" else s"Token($id,$lexeme)"
}

final class FileToken(id : String, lexeme : String, startLoc : (Int, Int), endLoc : (Int, Int))
  extends Token(id, lexeme) {
  override def toString = if (id == lexeme) s"Token($id,start=$startLoc,end=$endLoc)" else s"Token($id,$lexeme,start=$startLoc,end=$endLoc)"
}

class TokenBuilder {
  def create(id : String, lexeme : String) : Token = new Token(id, lexeme)
}

final class FileTokenBuilder extends TokenBuilder {
  var line = 1
  var column = 1

  override def create(id : String, lexeme : String) : Token = {
    val startLine = line
    val startColumn = column
    lexeme.foreach {
      case '\n' => line += 1; column = 1
      case _ => column += 1
    }
    new FileToken(id, lexeme, (startLine, startColumn), (line, column))
  }
}

trait Scanner extends Iterator[Token]

trait ScannerBuilder {
  def create(source : CharSource, tokenBuilder : TokenBuilder) : Scanner

  private final var nextPriority = 0
  private final var regexNFAs : List[TokenizedNFA] = Nil

  def token(id : String, pattern : String) : this.type = {
    regexNFAs = TokenizedNFA.fromPattern(pattern, nextPriority, id) :: regexNFAs
    nextPriority += 1
    this
  }

  protected lazy val dfaEmulator : TokenizedDFAEmulator = {
    val resultNFA = regexNFAs.tail.fold(regexNFAs.head){(r,nfa)=>r | nfa}
    resultNFA.toEmulator.toDFAEmulator.optimized
  }
}