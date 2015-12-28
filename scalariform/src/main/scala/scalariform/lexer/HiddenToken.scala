package scalariform.lexer

abstract sealed class HiddenToken(val token: Token) {

  lazy val newlineful = token.text contains '\n'

  def text = token.text

  def rawText = token.rawText

}

case class Whitespace(override val token: Token) extends HiddenToken(token)

sealed abstract class Comment(token: Token) extends HiddenToken(token)

object Comment {

  def unapply(comment: Comment) = Some(comment.token)

}

case class SingleLineComment(override val token: Token) extends Comment(token)

case class MultiLineComment(override val token: Token) extends Comment(token)

case class ScalaDocComment(override val token: Token) extends Comment(token)
