package scalariform.formatter

import scalariform.lexer.Token

case class FormatterState(indentLevel: Int = 0, indentRelativeToTokenOption: Option[Token] = None, val inSingleLineBlock: Boolean = false) {

  private val nextIndentLevel = indentLevel + 1

  def indent: FormatterState = indent(1)

  def indent(n: Int): FormatterState = copy(indentLevel = indentLevel + n)

  def alignWithToken(token: Token): FormatterState = copy(indentLevel = 0, indentRelativeToTokenOption = Some(token))

  def nextIndentLevelInstruction = EnsureNewlineAndIndent(nextIndentLevel, relativeTo = indentRelativeToTokenOption)

  def currentIndentLevelInstruction = EnsureNewlineAndIndent(indentLevel, relativeTo = indentRelativeToTokenOption)

}
