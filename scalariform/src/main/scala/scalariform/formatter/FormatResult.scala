package scalariform.formatter

import scalariform.lexer.Tokens._
import scalariform.lexer._
import scalariform.parser._
import scalariform.utils._

case class FormatResult(predecessorFormatting: Map[Token, IntertokenFormatInstruction],
                        inferredNewlineFormatting: Map[Token, IntertokenFormatInstruction],
                        xmlRewrites: Map[Token, String]) {

  def replaceXml(token: Token, replacement: String) = {
    require(token.getType.isXml)
    copy(xmlRewrites = xmlRewrites + (token -> replacement))
  }

  def before(token: Token, formatInstruction: IntertokenFormatInstruction) = {
    require(!token.isNewline, " cannot do 'before' formatting for NEWLINE* tokens: " + token + ", " + formatInstruction)
    copy(predecessorFormatting = predecessorFormatting + (token -> formatInstruction))
  }

  def formatNewline(token: Token, formatInstruction: IntertokenFormatInstruction) = {
    require(token.isNewline, " cannot do 'newline' formatting for non-NEWLINE tokens: " + token + ", " + formatInstruction)
    copy(inferredNewlineFormatting = inferredNewlineFormatting + (token -> formatInstruction))
  }

  def formatNewlineOrOrdinary(token: Token, formatInstruction: IntertokenFormatInstruction) =
    if (token.isNewline) formatNewline(token, formatInstruction)
    else before(token, formatInstruction)

  def mergeWith(other: FormatResult): FormatResult =
    FormatResult(this.predecessorFormatting ++ other.predecessorFormatting,
      this.inferredNewlineFormatting ++ other.inferredNewlineFormatting,
      this.xmlRewrites ++ other.xmlRewrites)

  def ++(other: FormatResult) = mergeWith(other)
}

object NoFormatResult extends FormatResult(Map(), Map(), Map())

abstract sealed class IntertokenFormatInstruction

/** Packs the comments together as compactly as possible, eliminating
 * as much non-comment whitespace as possible while ensuring that the
 * lexer produces the same tokens.*/
case object Compact extends IntertokenFormatInstruction

/** Like "Compact", but ensures there is either some comment or a single space. */
case object CompactEnsuringGap extends IntertokenFormatInstruction

/** Like "Compact", but will keep at least a single space if there was whitespace before */
case object CompactPreservingGap extends IntertokenFormatInstruction

/** Ensures that the interttoken region ends with NEWLINE INDENT. */
case class EnsureNewlineAndIndent(indentLevel: Int, relativeTo: Option[Token] = None) extends IntertokenFormatInstruction
