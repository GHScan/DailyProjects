package utils

object Characters {
  val All = (0 to 127).map(_.toChar)
  val Letters = All.filter(_.isLetter)
  val Uppers = All.filter(_.isUpper)
  val Lowers = All.filter(_.isLower)
  val Digits = All.filter(_.isDigit)
  val LetterOrDigits = All.filter(_.isLetterOrDigit)
  val Spaces = All.filter(_.isSpaceChar)
  val NoneLetters = All.filter(!_.isLetter)
  val NoneDigits = All.filter(!_.isDigit)
  val NoneLetterOrDigits = All.filter(!_.isLetterOrDigit)
  val NoneSpaces = All.filter(!_.isSpaceChar)
}