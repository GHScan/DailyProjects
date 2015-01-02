package lexical

trait IToken {
  def id : String
}

final case class Token(id : String) extends IToken {
  override def toString = s"Token($id)"
}

final case class TokenExt(id : String, value : Any) extends IToken {
  override def toString = s"Token($id,$value)"
}

final case class FileToken(id : String, startLoc : (Int, Int), endLoc : (Int, Int)) extends IToken {
  override def toString = s"Token($id,start=$startLoc,end=$endLoc)"
}

final case class FileTokenExt(id : String, value : Any, startLoc : (Int, Int), endLoc : (Int, Int)) extends IToken {
  override def toString = s"Token($id,$value,start=$startLoc,end=$endLoc)"
}

trait ITokenBuilder {
  def create(id : String, lexeme : String) : IToken

  def createExt(id : String, lexeme : String, value : Any) : IToken

  def error(lexeme : String) : IToken
}

final class TokenBuilder extends ITokenBuilder {
  def create(id : String, lexeme : String) : IToken = Token(id)

  def createExt(id : String, lexeme : String, value : Any) : IToken = TokenExt(id, value)

  def error(lexeme : String) : IToken = throw new Exception(s"Invalid token: $lexeme")
}

final class FileTokenBuilder extends ITokenBuilder {
  var line = 1
  var column = 1

  private def nextLocation(lexeme : String) : ((Int, Int), (Int, Int)) = {
    val startLoc = (line, column)
    lexeme.foreach {
      case '\n' => line += 1; column = 1
      case _ => column += 1
    }
    (startLoc, (line, column))
  }

  def create(id : String, lexeme : String) : IToken = {
    val (startLoc, endLoc) = nextLocation(lexeme)
    FileToken(id, startLoc, endLoc)
  }

  def createExt(id : String, lexeme : String, value : Any) : IToken = {
    val (startLoc, endLoc) = nextLocation(lexeme)
    FileTokenExt(id, value, startLoc, endLoc)
  }

  def error(lexeme : String) : IToken = throw new Exception(s"Invalid token: $lexeme ($line,$column)")
}

trait IScanner extends Iterator[IToken]

class TokenFAStateAttribute(val priority : Int, val id : String, val lexemeHandler : String => Any) extends IFAStateAttribute {
  override def toString = id
}

trait ScannerBuilder {
  def create(source : ICharSource, tokenBuilder : ITokenBuilder) : IScanner

  private final var nextPriority = 0
  private final var regexNFAs : List[TokenizedNFA] = Nil

  def token(id : String, pattern : String, lexemeHandler : String => Any) : this.type = {
    regexNFAs = TokenizedNFA.fromPattern(pattern, new TokenFAStateAttribute(nextPriority, id, lexemeHandler)) :: regexNFAs
    nextPriority -= 1
    this
  }

  def token(id : String) : this.type = {
    token(id, RegexAST.escape(id), _ => null)
  }

  protected lazy val dfaEmulator : TokenizedDFAEmulator = {
    val resultNFA = regexNFAs.tail.fold(regexNFAs.head) { (r, nfa) => r | nfa}
    resultNFA.toEmulator.toDFAEmulator.optimized
  }
}