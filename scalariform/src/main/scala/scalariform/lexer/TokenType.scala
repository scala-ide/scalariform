package scalariform.lexer

case class TokenType(name: String, isXml: Boolean = false) {

  def isNewline: Boolean = this == Tokens.NEWLINE || this == Tokens.NEWLINES

  def isKeyword: Boolean = Tokens.KEYWORDS contains this

  def isComment: Boolean = Tokens.COMMENTS contains this

  def isId: Boolean = Tokens.IDS contains this

  def isLiteral: Boolean = Tokens.LITERALS contains this

  override lazy val toString: String = name

}
