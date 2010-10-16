package scalariform.lexer

case class Token(tokenType: TokenType, text: String, startIndex: Int, stopIndex: Int) {
  require(tokenType == Tokens.EOF || stopIndex - startIndex + 1 == text.length)
  lazy val getText = text // Delete me?
  lazy val getType = tokenType // Delete me?
  lazy val getLine = -1 // TODO
  lazy val getCharPositionInLine = -1 // TODO
  lazy val getStartIndex = startIndex // Delete me?
  lazy val getStopIndex = stopIndex // Delete me?

  def isNewline = tokenType.isNewline
}
