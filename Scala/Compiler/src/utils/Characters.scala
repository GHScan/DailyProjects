package utils

object Characters {
  val All = (0 to 127).map(_.toChar)
  val Letters = All.filter(_.isLetter)
  val Uppers = All.filter(_.isUpper)
  val Lowers = All.filter(_.isLower)
  val Digits = All.filter(_.isDigit)
  val LetterOrDigits = All.filter(c => c.isLetterOrDigit || c == '_')
  val Whitespaces = All.filter(_.isWhitespace)
  val NoneLetters = All.filter(!_.isLetter)
  val NoneDigits = All.filter(!_.isDigit)
  val NoneLetterOrDigits = All.filter(c => !c.isLetterOrDigit && c != '_')
  val NoneWhitespaces = All.filter(!_.isWhitespace)
}