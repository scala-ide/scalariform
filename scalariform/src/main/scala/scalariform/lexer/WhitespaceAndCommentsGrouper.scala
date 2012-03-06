package scalariform.lexer

import scalariform.lexer.Tokens._
import scala.collection.mutable.ListBuffer

/**
 * Groups together whitespace and comments and filters them out from other token types.
 */
private[lexer] class WhitespaceAndCommentsGrouper(lexer: ScalaLexer) extends Iterator[(HiddenTokens, Token)] {

  private var nextToken = lexer.nextToken()

  private var ended = false

  def hasNext = !ended

  def next() = {
    require(hasNext)
    val hiddenTokens = readHiddenTokens()
    val resultToken = nextToken
    if (nextToken.tokenType == EOF)
      ended = true
    nextToken = lexer.nextToken()
    (hiddenTokens, resultToken)
  }

  private def readHiddenTokens(): HiddenTokens = {
    val hiddenTokens = new ListBuffer[HiddenToken]
    while (isCommentOrWhitespace(nextToken)) {
      hiddenTokens += makeHiddenToken(nextToken)
      nextToken = lexer.nextToken()
    }
    new HiddenTokens(hiddenTokens.toList)
  }

  private def isCommentOrWhitespace(token: Token) = token.tokenType match {
    case WS | LINE_COMMENT | MULTILINE_COMMENT ⇒ true
    case _                                     ⇒ false
  }

  private def makeHiddenToken(token: Token) = token.tokenType match {
    case LINE_COMMENT ⇒ SingleLineComment(token)
    case MULTILINE_COMMENT if token.isScalaDocComment ⇒ ScalaDocComment(token)
    case MULTILINE_COMMENT ⇒ MultiLineComment(token)
    case WS ⇒ Whitespace(token)
  }

}
