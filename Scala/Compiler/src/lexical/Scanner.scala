package lexical

import scala.collection.immutable

trait IToken extends Ordered[IToken] {
  def id : Int
  def name : String
  def value : Any
  override def equals(other : Any) = other match {
    case o : IToken => id == o.id
    case _ => false
  }
  override def hashCode() = id.hashCode()
  def compare(that : IToken) : Int = id.compare(that.id)
}

object IToken {

  val Eof = new Token(0, "EOF", null)
  val Error = new Token(1, "ERROR", null)
  val Empty = new Token(2, "ε", null)

  val FirstTokenID = 3

}

final class Token(val id : Int, val name : String, val value : Any) extends IToken {
  override def toString = s"Token($name,$value)"
}

final class FileToken(val id : Int, val name : String, val value : Any, val startLoc : (Int, Int), val endLoc : (Int, Int)) extends IToken {
  override def toString = s"Token($name,$value,start=$startLoc,end=$endLoc)"
}

trait ITokenFactory {
  def create(id : Int, name : String, value : Any, lexeme : String) : IToken
  def eof() : IToken
  def error(lexeme : String) : IToken
}

final class TokenFactory extends ITokenFactory {
  def create(id : Int, name : String, value : Any, lexeme : String) : IToken = new Token(id, name, value)
  def eof() : IToken = IToken.Eof
  def error(lexeme : String) : IToken = new Token(IToken.Error.id, IToken.Error.name, lexeme)
}

final class FileTokenFactory extends ITokenFactory {
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

  def create(id : Int, name : String, value : Any, lexeme : String) : IToken = {
    val (startLoc, endLoc) = nextLocation(lexeme)
    new FileToken(id, name, value, startLoc, endLoc)
  }

  def eof() : IToken = IToken.Eof

  def error(lexeme : String) : IToken = {
    val (startLoc, endLoc) = nextLocation(lexeme)
    new FileToken(IToken.Error.id, IToken.Error.name, lexeme, startLoc, endLoc)
  }
}

trait IScanner extends Iterator[IToken]

class TokenFAStateAttribute(
  val priority : Int,
  val id : Int,
  val name : String,
  val lexemeHandler : (String) => Any) extends IFAStateAttribute {
  override def toString = name
}

trait ScannerBuilder {
  def create(source : ICharSource, tokenFactory : ITokenFactory) : IScanner

  private final var nextPriority = 0
  private final var nextTokenID = IToken.FirstTokenID
  private final var name2Token = immutable.Map[String, IToken]()
  private final var regexNFAs : List[TokenizedNFA] = Nil

  def token(name : String, pattern : String, lexemeHandler : (String) => Any) : this.type = {
    regexNFAs = TokenizedNFA.fromPattern(pattern, new TokenFAStateAttribute(nextPriority, nextTokenID, name, lexemeHandler)) :: regexNFAs
    name2Token += ((name, new Token(nextTokenID, name, null)))
    nextPriority -= 1
    nextTokenID += 1
    this
  }

  def token(name : String) : this.type = token(name, RegexAST.escape(name), identity)

  def literals(names : String*) : this.type = {
    names.foreach(token)
    this
  }

  def getToken(name : String) = name2Token(name)

  protected lazy val dfaEmulator : TokenizedDFAEmulator = {
    val resultNFA = regexNFAs.tail.fold(regexNFAs.head) { (r, nfa) => r | nfa}
    resultNFA.toDFAEmulator.optimized
  }

  object Implicits {
    implicit def string2Token(name : String) : IToken = getToken(name)
  }
}