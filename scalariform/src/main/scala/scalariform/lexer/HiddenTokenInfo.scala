package scalariform.lexer

trait HiddenTokenInfo {

  def isInferredNewline(token: Token): Boolean

  def inferredNewlines(token: Token): Option[HiddenTokens]

  def hiddenPredecessors(token: Token): HiddenTokens

  def allHiddenTokens: Iterable[HiddenTokens]

}