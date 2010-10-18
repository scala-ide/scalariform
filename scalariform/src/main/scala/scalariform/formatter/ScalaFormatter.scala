package scalariform.formatter

import scalariform.lexer.Tokens._
import scalariform.lexer._
import scalariform.parser._
import scalariform.utils.Utils._
import scalariform.utils._
import scalariform.utils.BooleanLang._

import scalariform.formatter.preferences._
import PartialFunction._

trait HasHiddenTokenInfo {

  def isInferredNewline(token: Token): Boolean

  def inferredNewlines(token: Token): HiddenTokens

  def hiddenPredecessors(token: Token): HiddenTokens

}

abstract class ScalaFormatter extends HasFormattingPreferences with TypeFormatter with AnnotationFormatter with ExprFormatter with HasHiddenTokenInfo with TemplateFormatter with XmlFormatter {

  val newlineSequence: String

  def format(compilationUnit: CompilationUnit)(implicit formatterState: FormatterState = FormatterState()): FormatResult = {
    val topStats = compilationUnit.topStats
    var result = format(topStats)
    for (firstStat ← topStats.firstStatOpt)
      result = result.before(firstStat.firstToken, EnsureNewlineAndIndent(0))
    result
  }

  private def alterSuspendFormatting(text: String): Option[Boolean] =
    if (text contains "format: OFF")
      Some(true)
    else if (text contains "format: ON")
      Some(false)
    else
      None

  private def replaceEdit(token: Token, replacement: String): TextEdit = TextEdit(token.startIndex, token.text.length, replacement)

  def writeTokens(s: String, tokens: List[Token], formatResult: FormatResult): List[TextEdit] = {
    val FormatResult(predecessorFormatting, inferredNewlineFormatting, xmlRewrites) = formatResult
    val builder = new StringBuilder
    var tokenIndentMap: Map[Token, Int] = Map()
    var suspendFormatting = false
    var edits: List[TextEdit] = Nil // Stored in reverse
    for ((previousTokenOption, token, nextTokenOption) ← Utils.withPreviousAndNext(tokens)) {
      val previousTokenIsPrintable = previousTokenOption exists { !isInferredNewline(_) }
      if (isInferredNewline(token)) {
        alterSuspendFormatting(token.getText) foreach { suspendFormatting = _ }
        if (suspendFormatting)
          builder.append(token.getText)
        else {
          val formattingInstruction = inferredNewlineFormatting.get(token) getOrElse
            defaultNewlineFormattingInstruction(previousTokenOption, token, nextTokenOption)
          val nextTokenUnindents = nextTokenOption exists { _.getType == RBRACE }
          val nextTokenIsPrintable = nextTokenOption exists { !isInferredNewline(_) }
          edits :::= writeHiddenTokens(builder, inferredNewlines(token), formattingInstruction, nextTokenUnindents, nextTokenIsPrintable, previousTokenIsPrintable, tokenIndentMap).toList
        }
      } else {
        val formattingInstruction = predecessorFormatting.get(token) orElse
          previousTokenOption.map(defaultFormattingInstruction(_, token)) getOrElse
          Compact
        alterSuspendFormatting(hiddenPredecessors(token).text) foreach { suspendFormatting = _ }
        if (suspendFormatting) {
          builder.append(hiddenPredecessors(token).text)
          tokenIndentMap += (token -> builder.currentIndent)
          builder.append(token.getText)
        } else {
          val nextTokenUnindents = token.getType == RBRACE
          val nextTokenIsPrintable = true // <-- i.e. current token
          val hiddenTokens = hiddenPredecessors(token)
          val positionHintOption = if (hiddenTokens.isEmpty) Some(token.startIndex) else None
          edits :::= writeHiddenTokens(builder, hiddenTokens, formattingInstruction, nextTokenUnindents, nextTokenIsPrintable, previousTokenIsPrintable, tokenIndentMap, positionHintOption).toList
          tokenIndentMap += (token -> builder.currentIndent)
          val newTokenTextOpt: Option[String] = if (xmlRewrites contains token) Some(xmlRewrites(token)) else None
          edits :::= builder.write(token, newTokenTextOpt).toList
        }
      }
    }
    edits.reverse filter { case TextEdit(start, length, replacement) ⇒ s.substring(start, start + length) != replacement } distinct
  }

  private def formatComment(comment: HiddenToken, indentLevel: Int) = {

    object StarLine {
      def unapply(line: String): Option[String] = {
        val isStarLine = line matches """^\s*\*.*"""
        if (isStarLine) {
          Some(line.substring(line.indexOf('*'), line.length))
        } else
          None
      }
    }

    val sb = new StringBuilder
    val lines = comment.getText split "\r?\n"
    for ((previousOption, line) ← Utils.pairWithPrevious(lines)) {
      line match {
        case StarLine(fromStar) ⇒ sb.append(newlineSequence).indent(indentLevel).append(" ").append(fromStar)
        case _ ⇒ sb.append(line)
      }
    }
    sb.toString
  }

  private def writeHiddenTokens(builder: StringBuilder,
                                hiddenTokens: HiddenTokens,
                                instruction: IntertokenFormatInstruction,
                                nextTokenUnindents: Boolean,
                                nextTokenIsPrintable: Boolean,
                                previousTokenIsPrintable: Boolean,
                                tokenIndentMap: Map[Token, Int],
                                positionHintOption: Option[Int] = None): Option[TextEdit] = {
    def writeIntertokenCompact() {
      val comments = hiddenTokens.comments
      for ((previousCommentOption, comment) ← Utils.pairWithPrevious(comments)) {
        val needGapBetweenThisAndPrevious = previousCommentOption match {
          case Some(MultiLineComment(_)) | Some(ScalaDocComment(_)) ⇒ true
          case _ if comment == comments.head && previousTokenIsPrintable ⇒ true
          case _ ⇒ false
        }
        if (needGapBetweenThisAndPrevious)
          builder.append(" ")
        builder.write(comment.token)
      }
      val needGapBetweenThisAndFollowing = comments.lastOption match {
        case Some(SingleLineComment(_)) ⇒ false
        case Some(MultiLineComment(_)) if nextTokenIsPrintable ⇒ true
        case Some(ScalaDocComment(_)) if nextTokenIsPrintable ⇒ true
        case _ ⇒ false
      }
      if (needGapBetweenThisAndFollowing)
        builder.append(" ")
    }

    val startPos = builder.length
    val allWhitespace = hiddenTokens forall { _.isInstanceOf[Whitespace] }
    var edits: List[TextEdit] = Nil
    instruction match {
      case Compact ⇒ writeIntertokenCompact()
      case CompactEnsuringGap ⇒
        if (allWhitespace)
          builder.append(" ")
        else
          writeIntertokenCompact()
      case CompactPreservingGap ⇒
        if (allWhitespace && !hiddenTokens.isEmpty)
          builder.append(" ")
        else
          writeIntertokenCompact()
      case EnsureNewlineAndIndent(indentLevel, relativeTo) ⇒
        val baseIndentOption = relativeTo flatMap tokenIndentMap.get
        if (hiddenTokens.isEmpty) {
          builder.ensureAtBeginningOfLine()
          builder.indent(indentLevel, baseIndentOption)
        } else {
          val commentIndentLevel = if (nextTokenUnindents) indentLevel + 1 else indentLevel
          for ((previousOpt, hiddenToken, nextOpt) ← Utils.withPreviousAndNext(hiddenTokens)) {
            hiddenToken match {
              case ScalaDocComment(token) ⇒
                builder.ensureAtBeginningOfLine()
                builder.indent(commentIndentLevel, baseIndentOption)
                builder.append(formatComment(hiddenToken, commentIndentLevel))
              case SingleLineComment(_) | MultiLineComment(_) ⇒
                if (builder.atBeginningOfLine)
                  builder.indent(commentIndentLevel, baseIndentOption)
                else if (builder.atVisibleCharacter) // Separation from previous visible token
                  builder.append(" ")
                builder.write(hiddenToken)
              case Whitespace(token) ⇒
                val newlineCount = token.getText.count(_ == '\n')
                val newlinesToWrite = previousOpt match {
                  case Some(SingleLineComment(_)) ⇒ math.min(1, newlineCount)
                  case _ ⇒ math.min(2, newlineCount)
                }
                for (i ← 1 to newlinesToWrite)
                  builder.newline()
            }
            if (nextOpt.isEmpty) {
              hiddenToken match {
                case ScalaDocComment(_) ⇒
                  builder.newline()
                  builder.indent(indentLevel, baseIndentOption)
                case SingleLineComment(_) ⇒
                  builder.indent(indentLevel, baseIndentOption)
                case MultiLineComment(_) ⇒
                  builder.append(" ")
                case Whitespace(token) ⇒
                  if (previousOpt.exists(_.isInstanceOf[MultiLineComment]) && !token.getText.contains('\n'))
                    builder.append(" ")
                  else {
                    builder.ensureAtBeginningOfLine()
                    builder.indent(indentLevel, baseIndentOption)
                  }
              }
            }

          }
        }
    }
    val replacement = builder.substring(startPos)
    positionHintOption match {
      case Some(positionHint) if hiddenTokens.isEmpty ⇒ Some(TextEdit(positionHint, length = 0, replacement = replacement))
      case _ ⇒
        for {
          firstToken ← hiddenTokens.firstTokenOption
          lastToken ← hiddenTokens.lastTokenOption
          val start = firstToken.token.startIndex
          val end = lastToken.token.stopIndex
          val length = end - start + 1
        } yield TextEdit(start, length, replacement)
    }
  }

  class StringBuilderExtra(builder: StringBuilder) {

    def indent(indentLevel: Int, baseIndentOption: Option[Int] = None) = {
      for {
        baseIndent ← baseIndentOption
        n ← 1 to baseIndent
      } builder.append(" ")
      val indentChars = Spaces(formattingPreferences(IndentSpaces)).indent(indentLevel)
      builder.append(indentChars)
      builder
    }

    def write(token: Token, replacementOption: Option[String] = None): Option[TextEdit] = {
      val rewriteArrows = formattingPreferences(RewriteArrowSymbols)
      val actualReplacementOption = replacementOption orElse (condOpt(token.getType) {
        case ARROW if rewriteArrows ⇒ "⇒"
        case LARROW if rewriteArrows ⇒ "←"
        case EOF ⇒ ""
      })
      builder.append(actualReplacementOption getOrElse token.getText)
      actualReplacementOption map { replaceEdit(token, _) }
    }

    def write(hiddenToken: HiddenToken) = {
      builder.append(hiddenToken.token.getText)
      builder
    }

    def newline() = {
      builder.append(newlineSequence)
      builder
    }

    def atBeginningOfLine = builder.length == 0 || lastChar == '\n'

    private def lastChar = builder.charAt(builder.length - 1)

    def currentIndent = {
      var pos = builder.length - 1
      while (pos >= 0 && builder.charAt(pos) != '\n')
        pos -= 1
      builder.length - pos - 1
    }

    def lastCharacter = if (builder.length == 0) None else Some(lastChar)

    def ensureAtBeginningOfLine() = {
      if (!atBeginningOfLine)
        newline()
      builder
    }

    def atVisibleCharacter = builder.length > 0 && !Character.isWhitespace(lastChar)

  }
  implicit def stringBuilder2stringBuilderExtra(builder: StringBuilder): StringBuilderExtra = new StringBuilderExtra(builder)

  private def defaultNewlineFormattingInstruction(previousTokenOption: Option[Token], token: Token, nextTokenOption: Option[Token]): IntertokenFormatInstruction = {
    val previousTypeOption = previousTokenOption map { _.getType }
    val nextTypeOption = nextTokenOption map { _.getType }
    val result =
      if (previousTypeOption == Some(TYPE))
        CompactEnsuringGap
      else if (previousTypeOption == Some(RBRACKET) && nextTypeOption.exists(Set(CASE, CLASS, TRAIT, OBJECT, DEF, VAL, VAR, TYPE, ABSTRACT, FINAL, SEALED, OVERRIDE, IMPLICIT, LAZY)))
        CompactEnsuringGap
      else if (nextTypeOption == Some(LBRACE))
        CompactEnsuringGap
      else
        Compact
    // println("defaultNewlineFormattingInstruction(" + previousTokenOption + ", " + token + ", " + nextTokenOption + ") = " + result)
    result
  }

  private def defaultFormattingInstruction(token1: Token, token2: Token): IntertokenFormatInstruction = {
    val result = actualDefaultFormattingInstruction(token1, token2)
    //    println("defaultFormattingInstruction(" + token1 + ", " + token2 + ") = " + result)
    result
  }
  private def actualDefaultFormattingInstruction(token1: Token, token2: Token): IntertokenFormatInstruction = {
    import ScalaFormatter._
    import scalariform.lexer.ScalaOnlyLexer.isOperatorPart
    val type1 = token1.getType
    val type2 = token2.getType
    if (type2 == EOF)
      return Compact
    val xmlPreviousExceptions = Set(LBRACE, LPAREN, NEWLINE, NEWLINES)
    if (type2 == XML_START_OPEN && !(xmlPreviousExceptions.contains(type1) || type1.isXml))
      return CompactEnsuringGap
    if (type1 == USCORE && IDS.contains(type2) && type2 != STAR)
      return CompactEnsuringGap
    if (type2 == USCORE && IDS.contains(type1))
      return CompactEnsuringGap
    if ((type1 == RPAREN || type1 == RBRACKET) && type2 == LBRACE)
      return CompactEnsuringGap
    if (type1 == MINUS && (type2 == INTEGER_LITERAL || type2 == FLOATING_POINT_LITERAL))
      return Compact
    if (Set(IMPLICIT, VAL, VAR, PRIVATE, PROTECTED, OVERRIDE).contains(type2) && type1 == LPAREN)
      return Compact
    if ((type1 == PROTECTED || type1 == PRIVATE) && type2 == LBRACKET)
      return Compact
    if (type1 == NEWLINE || type2 == NEWLINE || type1 == NEWLINES || type2 == NEWLINES)
      return Compact
    if (IDS.contains(type1) && type2 == LBRACE)
      return CompactEnsuringGap
    if (type1 == LBRACE && type2 == RBRACE)
      return Compact //CompactEnsuringGap
    if (type1 == RBRACE && type2 == LBRACE)
      return CompactEnsuringGap
    if (type1 == RPAREN && LITERALS.contains(type2))
      return CompactEnsuringGap
    if (type1 == RPAREN && IDS.contains(type2))
      return CompactEnsuringGap
    if (LITERALS.contains(type1) && IDS.contains(type2))
      return CompactEnsuringGap
    if (IDS.contains(type1) && LITERALS.contains(type2))
      return CompactEnsuringGap
    if (ENSURE_SPACE_AFTER(type1))
      return CompactEnsuringGap
    if (ENSURE_SPACE_BEFORE(type2))
      return CompactEnsuringGap
    if (IDS.contains(type1) && IDS.contains(type2))
      return CompactEnsuringGap
    val firstCharOfToken2 = token2.getText.head
    if (Set(HASH, AT).contains(type1) && isOperatorPart(firstCharOfToken2))
      return CompactEnsuringGap
    val lastCharOfToken1 = token1.getText.last
    val firstIsIdEndingWithOpChar = IDS.contains(type1) && (lastCharOfToken1 == '_' || isOperatorPart(lastCharOfToken1))
    if (Set(HASH, COLON, AT).contains(type2) && firstIsIdEndingWithOpChar)
      return CompactEnsuringGap
    if (type2 == COLON && formattingPreferences(SpaceBeforeColon))
      return CompactEnsuringGap
    type1 match {
      case ARROW if type2 != RPAREN ⇒ return CompactEnsuringGap // TODO: Redundant? no test fails.
      case COMMA ⇒ return CompactEnsuringGap
      case _ ⇒
    }
    type2 match {
      case IF if type1 != LPAREN ⇒ return CompactEnsuringGap
      case ARROW if type1 != LPAREN ⇒ return CompactEnsuringGap
      case AT if IDS contains type2 ⇒ return CompactEnsuringGap
      case _ ⇒
    }
    Compact
  }

  protected def containsNewline(tokens: List[Token]): Boolean =
    tokens exists { token ⇒
      require(token != null)
      require(token.getText != null, token)
      token != tokens.head && hiddenPredecessors(token).containsNewline ||
        token.getText.contains("\n") ||
        isInferredNewline(token) && inferredNewlines(token).containsNewline // TODO: Why would an inferred newline not contain newline?
    }

  protected def containsNewline(astNode: AstNode): Boolean = containsNewline(astNode.tokens)

}

object ScalaFormatter {

  val IDS = Set(VARID, OTHERID, PLUS, MINUS, STAR, PIPE, TILDE, EXCLAMATION)

  val LITERALS = Set(CHARACTER_LITERAL, INTEGER_LITERAL, FLOATING_POINT_LITERAL, STRING_LITERAL, SYMBOL_LITERAL, TRUE, FALSE, NULL)

  // format: OFF
  val ENSURE_SPACE_AFTER = Set(
    ABSTRACT, CASE, CATCH, CLASS, DEF,
    DO, ELSE, EXTENDS, FINAL,
    FINALLY, FOR, FORSOME, IF, IMPLICIT,
    IMPORT, LAZY, MATCH, NEW,
    OBJECT, OVERRIDE, PACKAGE, PRIVATE, PROTECTED,
    REQUIRES, RETURN, SEALED, /* SUPER, THIS, */
    THROW, TRAIT, TRY, TYPE,
    VAL, VAR, WHILE, WITH, YIELD, 
    /* USCORE, */ COLON, EQUALS, ARROW, LARROW, SUBTYPE, VIEWBOUND, SUPERTYPE, /* HASH, AT */
    LBRACE, SEMI)

  val ENSURE_SPACE_BEFORE = Set(
    ABSTRACT, CASE, CATCH, CLASS, DEF, 
    /* DO, */  ELSE, EXTENDS, FINAL,
    FINALLY, /* FOR, */ FORSOME, /* IF, */ IMPLICIT, 
    /* IMPORT, */ LAZY, MATCH, /* NEW, */
    OBJECT, OVERRIDE, /* PACKAGE, */ PRIVATE, PROTECTED,
    /* REQUIRES, RETURN, */ SEALED, /* SUPER, THIS, */
    /* THROW, */ TRAIT, /* TRY, TYPE, */
    VAL, VAR, /* WHILE, */ WITH, YIELD, 
    /* USCORE, COLON, */ EQUALS, /* ARROW, */ LARROW, SUBTYPE, VIEWBOUND, SUPERTYPE, /*, HASH, AT, */
    RBRACE)
  // format: ON

  @throws(classOf[ScalaParserException])
  def format(source: String, formattingPreferences: IFormattingPreferences = FormattingPreferences()): String = {
    val edits = formatAsEdits(source, formattingPreferences)
    TextEditProcessor.runEdits(source, edits)
  }

  @throws(classOf[ScalaParserException])
  def formatAsEdits(source: String, formattingPreferences: IFormattingPreferences, lineDelimiter: Option[String] = None): List[TextEdit] = {
    val specificFormatter = new SpecificFormatter {

      type Result = CompilationUnit

      def parse(parser: ScalaParser) = parser.compilationUnitOrScript()

      def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState(indentLevel = 0))

    }
    val (edits, _) = specificFormatter.fullFormat(source)(formattingPreferences)
    edits
  }

}
