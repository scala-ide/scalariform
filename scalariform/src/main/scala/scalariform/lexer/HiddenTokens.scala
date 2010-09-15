package scalariform.lexer

import scalariform.lexer.Tokens._
import scalariform.utils.Utils

class HiddenTokens(val tokens: List[HiddenToken]) extends Iterable[HiddenToken] {

  def removeInitialWhitespace = new HiddenTokens(tokens.dropWhile(_.isInstanceOf[Whitespace]))

  def iterator: Iterator[HiddenToken] = tokens.iterator

  val comments: List[Comment] = tokens collect { case comment@Comment(_) ⇒ comment }

  val whitespaces: List[Whitespace] = tokens collect { case whitespace@Whitespace(_) ⇒ whitespace }

  def firstTokenOption = tokens.headOption
  def lastTokenOption = tokens.lastOption

  def containsNewline = text contains '\n'

  def containsComment = !comments.isEmpty

  lazy val text = tokens.map(_.token.getText).mkString

  lazy val newlines: Option[Token] =
    if (containsNewline) {
      require(!tokens.isEmpty)
      val tokenType = if (text matches HiddenTokens.BLANK_LINE_PATTERN) NEWLINES else NEWLINE
      val first = tokens.head.token
      val last = tokens.last.token
      val token = new Token(tokenType, text, first.getStartIndex, last.getStopIndex)
      Some(token)
    } else
      None

}

object HiddenTokens {

  val BLANK_LINE_PATTERN = """(?s).*\n\s*\n.*"""

}

abstract sealed class HiddenToken(val token: Token) {
  lazy val newlineful = token.getText contains '\n'
  lazy val getText = token.getText
}

case class Whitespace(override val token: Token) extends HiddenToken(token)

abstract class Comment(token: Token) extends HiddenToken(token)

object Comment {
  def unapply(comment: Comment) = Some(comment.token)
}

case class SingleLineComment(override val token: Token) extends Comment(token)

case class MultiLineComment(override val token: Token) extends Comment(token)

case class ScalaDocComment(override val token: Token) extends Comment(token)
