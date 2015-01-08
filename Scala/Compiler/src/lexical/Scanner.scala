package lexical

import scala.collection.immutable

trait IToken {
  def id : Int
  override def equals(other : Any) = other match {
    case o : IToken => id == o.id
    case _ => false
  }
}

object IToken {

  val Empty = new IToken {
    val id = 0
    override def toString = "ε"
  }
  val Eof = new IToken {
    val id : Int = 1
    override def toString = "EOF"
  }
  val Error = new IToken {
    val id : Int = 2
    override def toString = "Error"
  }

  val FirstTokenID = 3

}

final class Token(val id : Int, val name : String) extends IToken {
  override def toString = s"Token($name)"
}
final class TokenExt(val id : Int, val name : String, val value : Any) extends IToken {
  override def toString = s"Token($name,$value)"
}

final class FileToken(val id : Int, val name : String, val startLoc : (Int, Int), val endLoc : (Int, Int)) extends IToken {
  override def toString = s"Token($name,start=$startLoc,end=$endLoc)"
}
final class FileTokenExt(val id : Int, val name : String, val value : Any, val startLoc : (Int, Int), val endLoc : (Int, Int)) extends IToken {
  override def toString = s"Token($name,$value,start=$startLoc,end=$endLoc)"
}

trait ITokenFactory {
  def create(id : Int, name : String, lexeme : String) : IToken
  def createExt(id : Int, name : String, lexeme : String, value : Any) : IToken
  def eof() : IToken
  def error(lexeme : String) : IToken
}

final class TokenFactory extends ITokenFactory {
  def create(id : Int, name : String, lexeme : String) : IToken = new Token(id, name)
  def createExt(id : Int, name : String, lexeme : String, value : Any) : IToken = new TokenExt(id, name, value)
  def eof() : IToken = IToken.Eof
  def error(lexeme : String) : IToken = new TokenExt(IToken.Error.id, IToken.Error.toString, lexeme)
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

  def create(id : Int, name : String, lexeme : String) : IToken = {
    val (startLoc, endLoc) = nextLocation(lexeme)
    new FileToken(id, name, startLoc, endLoc)
  }

  def createExt(id : Int, name : String, lexeme : String, value : Any) : IToken = {
    val (startLoc, endLoc) = nextLocation(lexeme)
    new FileTokenExt(id, name, value, startLoc, endLoc)
  }

  def eof() : IToken = IToken.Eof

  def error(lexeme : String) : IToken = {
    val (startLoc, endLoc) = nextLocation(lexeme)
    new FileTokenExt(IToken.Error.id, IToken.Error.toString, lexeme, startLoc, endLoc)
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
    name2Token += ((name, new Token(nextTokenID, name)))
    nextPriority -= 1
    nextTokenID += 1
    this
  }

  def token(name : String) : this.type = token(name, RegexAST.escape(name), _ => null)

  def lookupToken(name : String) = name2Token(name)

  protected lazy val dfaEmulator : TokenizedDFAEmulator = {
    val resultNFA = regexNFAs.tail.fold(regexNFAs.head) { (r, nfa) => r | nfa}
    resultNFA.toDFAEmulator.optimized
  }
}