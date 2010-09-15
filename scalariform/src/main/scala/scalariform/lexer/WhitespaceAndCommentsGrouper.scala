package scalariform.lexer

import scalariform.lexer.Tokens._
import scala.collection.mutable.ListBuffer

class WhitespaceAndCommentsGrouper(private val delegate: ScalaLexer) extends Iterator[(HiddenTokens, Token)] {

  private var currentToken = delegate.nextToken
  private var ended = false
  def hasNext = !ended

  def next() = {
    require(hasNext)
    val hiddenTokens = readHiddenTokens()
    val resultToken = currentToken
    if (currentToken.getType == EOF)
      ended = true
    currentToken = delegate.nextToken
    (hiddenTokens, resultToken)
  }

  private def readHiddenTokens(): HiddenTokens = {
    var hiddenTokens = new ListBuffer[HiddenToken]
    while (currentToken.getType != EOF && isHiddenToken(currentToken)) {
      hiddenTokens += makeHiddenToken(currentToken)
      currentToken = delegate.nextToken
    }
    new HiddenTokens(hiddenTokens.toList)
  }

  private def isHiddenToken(token: Token) = token.getType match {
    case WS | LINE_COMMENT | MULTILINE_COMMENT ⇒ true
    case _ ⇒ false
  }

  private def makeHiddenToken(token: Token) = token.getType match {
    case LINE_COMMENT ⇒ SingleLineComment(token)
    case MULTILINE_COMMENT if (token.getText.startsWith("/**") && token.getText != "/**/") ⇒ ScalaDocComment(token)
    case MULTILINE_COMMENT ⇒ MultiLineComment(token)
    case WS ⇒ Whitespace(token)
  }

}
