package scalariform.lexer

import scalariform.utils.Range
import scalariform.lexer.Tokens._

case class Token(tokenType: TokenType, text: String, startIndex: Int, stopIndex: Int) {
  require(tokenType == Tokens.EOF || stopIndex - startIndex + 1 == text.length)
  lazy val getText = text // Delete me?
  lazy val getType = tokenType // Delete me?
  lazy val getStartIndex = startIndex // Delete me?
  lazy val getStopIndex = stopIndex // Delete me?
  def length = stopIndex - startIndex + 1
  def isNewline = tokenType.isNewline
  def range = Range(startIndex, length)

  def isScalaDocComment = tokenType == MULTILINE_COMMENT && text.startsWith("/**") && text != "/**/"

}
