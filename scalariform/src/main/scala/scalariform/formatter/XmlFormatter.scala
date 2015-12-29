package scalariform.formatter

import scalariform.parser._
import scalariform.utils._
import scalariform.lexer._
import scalariform.formatter.preferences._

trait XmlFormatter { self: HasFormattingPreferences with ExprFormatter with ScalaFormatter ⇒

  def format(xmlExpr: XmlExpr)(implicit formatterState: FormatterState): FormatResult = {
    if (formattingPreferences(FormatXml)) {
      val XmlExpr(first, otherElements) = xmlExpr
      var formatResult: FormatResult = NoFormatResult
      formatResult ++= format(first)
      val nestedFormatterState =
        if (formattingPreferences(IndentWithTabs)) formatterState
        else formatterState.alignWithToken(first.firstToken)
      val (contentsFormatResult, _) = format(otherElements)(formatterState, nestedFormatterState)
      formatResult ++= contentsFormatResult
      formatResult
    } else
      NoFormatResult
  }

  def format(xmlContent: XmlContents)(implicit formatterState: FormatterState): FormatResult = {
    xmlContent match {
      case expr @ Expr(_)                            ⇒ format(expr)
      case xmlNonEmpty @ XmlNonEmptyElement(_, _, _) ⇒ format(xmlNonEmpty)
      case xmlEmpty @ XmlEmptyElement(_, _, _, _, _) ⇒ format(xmlEmpty)
      case _                                         ⇒ NoFormatResult // TODO
    }
  }

  def format(xmlEmpty: XmlEmptyElement)(implicit formatterState: FormatterState): FormatResult = {
    val XmlEmptyElement(_, _, attributes, whitespaceOption, _) = xmlEmpty
    var formatResult: FormatResult = NoFormatResult
    for ((whitespaceOption2, attribute) ← attributes) {
      for (whitespace ← whitespaceOption2 if formattingPreferences(FormatXml))
        formatResult = formatResult.replaceXml(whitespace, " ")
      formatResult ++= format(attribute)
    }
    for (whitespace ← whitespaceOption if formattingPreferences(FormatXml))
      formatResult = formatResult.replaceXml(whitespace, "")
    formatResult
  }

  // TODO: Dup with XmlEmptyElement
  def format(xmlStart: XmlStartTag)(implicit formatterState: FormatterState): FormatResult = {
    val XmlStartTag(_, _, attributes, whitespaceOption, _) = xmlStart
    var formatResult: FormatResult = NoFormatResult
    for ((whitespaceOption2, attribute) ← attributes) {
      for (whitespace ← whitespaceOption2 if formattingPreferences(FormatXml))
        formatResult = formatResult.replaceXml(whitespace, " ")
      formatResult ++= format(attribute)
    }
    for (whitespace ← whitespaceOption if formattingPreferences(FormatXml))
      formatResult = formatResult.replaceXml(whitespace, "")
    formatResult
  }

  def format(xmlEnd: XmlEndTag)(implicit formatterState: FormatterState): FormatResult = {
    val XmlEndTag(_, _, whitespaceOption, _) = xmlEnd
    var formatResult: FormatResult = NoFormatResult
    for (whitespace ← whitespaceOption if formattingPreferences(FormatXml))
      formatResult = formatResult.replaceXml(whitespace, "")
    formatResult
  }

  def format(xmlAttribute: XmlAttribute)(implicit formatterState: FormatterState): FormatResult = {
    val XmlAttribute(_, whitespaceOption, _, whitespaceOption2, valueOrEmbeddedScala) = xmlAttribute
    var formatResult: FormatResult = NoFormatResult
    for (whitespace ← whitespaceOption if formattingPreferences(FormatXml))
      formatResult = formatResult.replaceXml(whitespace, "")
    for (embeddedScala ← valueOrEmbeddedScala.right)
      formatResult ++= format(embeddedScala)
    for (whitespace ← whitespaceOption2 if formattingPreferences(FormatXml))
      formatResult = formatResult.replaceXml(whitespace, "")
    formatResult
  }

  def format(xmlNonEmpty: XmlNonEmptyElement)(implicit formatterState: FormatterState): FormatResult = {
    val XmlNonEmptyElement(startTag, contents, endTag) = xmlNonEmpty
    var formatResult: FormatResult = NoFormatResult
    formatResult ++= format(startTag)
    val nestedFormatterState =
      if (formattingPreferences(IndentWithTabs)) formatterState.indent
      else formatterState.alignWithToken(startTag.firstToken).indent
    val (contentsFormatResult, multiline) = format(contents)(formatterState, nestedFormatterState)
    formatResult ++= contentsFormatResult
    if (multiline) {
      val instruction =
        if (formattingPreferences(IndentWithTabs)) formatterState.currentIndentLevelInstruction
        else formatterState.alignWithToken(startTag.firstToken).currentIndentLevelInstruction
      formatResult = formatResult.before(endTag.firstToken, instruction)
    }
    formatResult ++= format(endTag)
    formatResult
  }

  /**
   * @return format result and whether the contents span multiple lines
   */
  def format(contents: List[XmlContents])(formatterState: FormatterState, nestedFormatterState: FormatterState): (FormatResult, Boolean) = {
    var formatResult: FormatResult = NoFormatResult
    val multiline = contents exists {
      /* case x: XmlElement ⇒ true */
      case XmlPCDATA(token) if token.text contains '\n' ⇒ true
      case _ ⇒ false
    }
    var firstNonWhitespace = true
    for (previousAndThis ← Utils.pairWithPrevious(contents)) {
      previousAndThis match {
        case (_, XmlPCDATA(token @ Token(_, Trimmed(prefix, infix, suffix), _, _))) ⇒
          if (infix == "") {
            if (multiline)
              formatResult = formatResult.replaceXml(token, "")
          } else {
            firstNonWhitespace = false
            if (multiline) {
              formatResult = formatResult.replaceXml(token, infix)
              formatResult = formatResult.before(token, nestedFormatterState.currentIndentLevelInstruction)
            }
          }
        case (_, xmlContent) if multiline && firstNonWhitespace ⇒
          firstNonWhitespace = false
          formatResult = formatResult.before(xmlContent.firstToken, nestedFormatterState.currentIndentLevelInstruction)
          formatResult ++= format(xmlContent)(nestedFormatterState)
        case (Some(XmlPCDATA(Token(_, Trimmed(prefix, infix, suffix), _, _))), xmlContent) if multiline && (suffix.contains('\n') || (infix == "" && prefix.contains('\n'))) ⇒
          firstNonWhitespace = false
          formatResult = formatResult.before(xmlContent.firstToken, nestedFormatterState.currentIndentLevelInstruction)
          formatResult ++= format(xmlContent)(nestedFormatterState)
        case (Some(_), xmlContent @ Expr(_)) ⇒
          firstNonWhitespace = false
          formatResult = formatResult.before(xmlContent.firstToken, Compact)
          formatResult ++= format(xmlContent)(if (multiline) nestedFormatterState else formatterState)
        case (Some(Expr(_)), xmlContent) ⇒
          firstNonWhitespace = false
          formatResult = formatResult.before(xmlContent.firstToken, Compact)
          formatResult ++= format(xmlContent)(formatterState)
        case (_, xmlContent) ⇒
          firstNonWhitespace = false
          formatResult ++= format(xmlContent)(formatterState)
      }
    }
    (formatResult, multiline)
  }

}
