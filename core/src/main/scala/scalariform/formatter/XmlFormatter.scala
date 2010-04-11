package scalariform.formatter

import scalariform.parser._
import scalariform.utils._
import scalariform.lexer._
import scalariform.formatter.preferences._
trait XmlFormatter { self: HasFormattingPreferences with ExprFormatter with ScalaFormatter ⇒

  def format(xmlExpr: XmlExpr)(implicit formatterState: FormatterState): FormatResult = {
    val XmlExpr(first: XmlContents, otherElements: List[XmlElement]) = xmlExpr
    var formatResult: FormatResult = NoFormatResult
    formatResult ++= format(first)
    for (element ← otherElements)
      formatResult ++= format(element)
    formatResult
  }

  def format(xmlContent: XmlContents)(implicit formatterState: FormatterState): FormatResult = {
    xmlContent match {
      case expr@Expr(_) ⇒ format(expr)
      case xmlNonEmpty@XmlNonEmptyElement(_, _, _) ⇒ format(xmlNonEmpty)
      case xmlEmpty@XmlEmptyElement(_, _, _, _, _) ⇒ format(xmlEmpty)
      case _ ⇒ NoFormatResult // TODO
    }
  }

  def format(xmlEmpty: XmlEmptyElement)(implicit formatterState: FormatterState): FormatResult = {
    val XmlEmptyElement(startOpen, name, attributes: List[(Option[Token], XmlAttribute)], whitespaceOption, emptyClose) = xmlEmpty
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
    val XmlStartTag(startOpen, name, attributes: List[(Option[Token], XmlAttribute)], whitespaceOption, tagClose) = xmlStart
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
  
  def format(xmlAttribute: XmlAttribute)(implicit formatterState: FormatterState): FormatResult = {
    val XmlAttribute(name, whitespaceOption, equals, whitespaceOption2, valueOrEmbeddedScala: Either[Token, Expr]) = xmlAttribute
    var formatResult: FormatResult = NoFormatResult
    for (whitespace ← whitespaceOption if formattingPreferences(FormatXml))
      formatResult = formatResult.replaceXml(whitespace, "")
    for (embeddedScala <- valueOrEmbeddedScala.right)
      formatResult ++= format(embeddedScala)
    for (whitespace ← whitespaceOption2 if formattingPreferences(FormatXml))
      formatResult = formatResult.replaceXml(whitespace, "")
    formatResult
  }

  def format(xmlNonEmpty: XmlNonEmptyElement)(implicit formatterState: FormatterState): FormatResult = {
    val XmlNonEmptyElement(startTag: XmlStartTag, contents: List[XmlContents], endTag: XmlEndTag) = xmlNonEmpty
    var formatResult: FormatResult = NoFormatResult
    val multiline = containsNewline(xmlNonEmpty)
    formatResult ++= format(startTag)
    
    for (xmlContent ← contents) {     
      formatResult ++= format(xmlContent)
    }
    if (multiline && formattingPreferences(FormatXml))
       formatResult = formatResult.before(endTag.firstToken, formatterState.alignWithToken(startTag.firstToken).currentIndentLevelInstruction)
    formatResult
  }

}

