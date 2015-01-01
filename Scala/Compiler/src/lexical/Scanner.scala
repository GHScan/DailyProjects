package lexical

trait IToken {
  def id : String
  def value : Any
}

final case class Token(id : String, value : Any) extends IToken {
  override def toString = if (id == value) s"Token($id)" else s"Token($id,${utils.Func.escape(value.toString)})"
}

final case class FileToken(id : String, value : Any, lexeme : String, startLoc : (Int, Int), endLoc : (Int, Int)) extends IToken {
  override def toString = if (id == value) s"Token($id,start=$startLoc,end=$endLoc)" else s"Token($id,${utils.Func.escape(lexeme)},start=$startLoc,end=$endLoc)"
}

trait ITokenBuilder {
  def create(id : String, value : Any, lexeme : String) : IToken
}

final class TokenBuilder extends ITokenBuilder {
  def create(id : String, value : Any, lexeme : String) : IToken = new Token(id, value)
}

final class FileTokenBuilder extends ITokenBuilder {
  var line = 1
  var column = 1

  override def create(id : String, value : Any, lexeme : String) : IToken = {
    val startLine = line
    val startColumn = column
    lexeme.foreach {
      case '\n' => line += 1; column = 1
      case _ => column += 1
    }
    new FileToken(id, value, lexeme, (startLine, startColumn), (line, column))
  }
}

trait IScanner extends Iterator[IToken]

class TokenStateAttribute(val priority : Int, val id : String, val handler : String=>Any) extends IStateAttribute {
  override def toString = id
}

trait ScannerBuilder {
  def create(source : ICharSource, tokenBuilder : ITokenBuilder) : IScanner

  private final var nextPriority = 0
  private final var regexNFAs : List[TokenizedNFA] = Nil

  def token(id : String, pattern : String, handler : String=>Any = identity) : this.type = {
    regexNFAs = TokenizedNFA.fromPattern(pattern, new TokenStateAttribute(nextPriority, id, handler)) :: regexNFAs
    nextPriority -= 1
    this
  }

  protected lazy val dfaEmulator : TokenizedDFAEmulator = {
    val resultNFA = regexNFAs.tail.fold(regexNFAs.head) { (r, nfa) => r | nfa}
    resultNFA.toEmulator.toDFAEmulator.optimized
  }
}