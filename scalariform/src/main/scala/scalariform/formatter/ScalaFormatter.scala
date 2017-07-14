package scalariform.formatter

import scalariform.lexer.Tokens._
import scalariform.lexer._
import scalariform.parser._
import scalariform.utils._
import scalariform.formatter.preferences._
import PartialFunction._
import scalariform.ScalaVersions

trait HasHiddenTokenInfo {

  def isInferredNewline(token: Token): Boolean

  def inferredNewlines(token: Token): HiddenTokens

  def hiddenPredecessors(token: Token): HiddenTokens

  def newlineBefore(token: Token): Boolean = hiddenPredecessors(token).containsNewline

  def newlineBefore(node: AstNode): Boolean = newlineBefore(node.firstToken)
}

abstract class ScalaFormatter
  extends HasFormattingPreferences
  with TypeFormatter
  with AnnotationFormatter
  with ExprFormatter
  with HasHiddenTokenInfo
  with TemplateFormatter
  with XmlFormatter
  with CaseClauseFormatter
  with CommentFormatter {

  val newlineSequence: String

  def getSource(astNode: AstNode): String = {
    val sb = new StringBuilder
    for (token ← astNode.tokens) {
      if (token != astNode.tokens.head)
        sb.append(hiddenPredecessors(token).rawText)
      sb.append(token.rawText)
    }
    sb.toString
  }

  def format(compilationUnit: CompilationUnit)(implicit formatterState: FormatterState = FormatterState()): FormatResult = {
    val topStats = compilationUnit.topStats
    var result = format(topStats)
    for (firstStat ← topStats.firstStatOpt)
      result = result.before(firstStat.firstToken, EnsureNewlineAndIndent(0))

    if (formattingPreferences(NewlineAtEndOfFile)) {
      result.before(compilationUnit.eofToken, EnsureNewlineAndIndent(0))
    } else {
      result
    }
  }

  /**
   * Converts an AstNode into what it should look like in text after Scalariform has run.
   * Useful for calculating the actual length of an [[scalariform.parser.AstNode]] after formatting.
   *
   * @param ast The AST to format and render as a string
   * @param astFormatResult Should run formatting actions for 'ast'
   * @return Formatted string representation of what the AstNode should look like after Scalariform
   *         has run
   */
  protected def formattedAstNode(ast: AstNode)(astFormatResult: ⇒ FormatResult): String = {
    val source = getSource(ast)
    val formatResult = astFormatResult
    val offset = ast.firstToken.offset
    val edits = writeTokens(source, ast.tokens, formatResult, offset)
    TextEditProcessor.runEdits(source, edits)
  }

  private def alterSuspendFormatting(text: String): Option[Boolean] =
    if (text contains "format: OFF")
      Some(true)
    else if (text contains "format: ON")
      Some(false)
    else
      None

  private def replaceEdit(token: Token, replacement: String): TextEdit = TextEdit(token.offset, token.length, replacement)

  def writeTokens(s: String, tokens: List[Token], formatResult: FormatResult, offset: Int = 0): List[TextEdit] = {
    val FormatResult(predecessorFormatting, inferredNewlineFormatting, xmlRewrites) = formatResult
    val builder = new StringBuilder
    var tokenIndentMap: Map[Token, Int] = Map()
    var suspendFormatting = false
    var edits: List[TextEdit] = Nil // Stored in reverse

    def printableFormattingInstruction(previousTokenOpt: Option[Token], token: Token) = {
      val isGaplessAssignment =
        predecessorFormatting.get(token) match { // avoid `foreach(_.id= ...)` gapless assignment (see MutateTest.scala)
          case Some(PlaceAtColumn(_, _, Some(Token(USCORE, _, _, _)))) if token.tokenType == EQUALS => true
          case _ => false
        }
      val maybeInstruction =
        if (isGaplessAssignment) Some(CompactEnsuringGap)
        else
          predecessorFormatting.get(token).orElse(
            previousTokenOpt.map(defaultFormattingInstruction(_, token))
          )
      maybeInstruction.getOrElse(
        if (token.tokenType == EOF) EnsureNewlineAndIndent(0) /* <-- to allow formatting of files with just a scaladoc comment */
        else Compact
      )
    }

    for ((previousTokenOption, token, nextTokenOption) ← Utils.withPreviousAndNext(tokens)) {
      val previousTokenIsPrintable = previousTokenOption exists { !isInferredNewline(_) }
      if (isInferredNewline(token)) {
        alterSuspendFormatting(token.text) foreach { suspendFormatting = _ }
        if (suspendFormatting)
          builder.append(token.rawText)
        else {
          val basicFormattingInstruction = inferredNewlineFormatting.get(token) getOrElse
            defaultNewlineFormattingInstruction(previousTokenOption, token, nextTokenOption)
          val formattingInstruction =
            if (nextTokenOption.exists { _.tokenType == EOF } && basicFormattingInstruction.isInstanceOf[EnsureNewlineAndIndent])
              EnsureNewlineAndIndent(0) // Adjustment for end of input when using non-zero initial indent
            else
              basicFormattingInstruction
          val nextTokenUnindents = nextTokenOption exists { _.tokenType == RBRACE }
          val includeBufferBeforeNextToken = nextTokenOption exists { nextToken ⇒
            !printableFormattingInstruction(Some(token), nextToken).isInstanceOf[EnsureNewlineAndIndent]
          }
          edits :::= writeHiddenTokens(builder, inferredNewlines(token), formattingInstruction, nextTokenUnindents,
            includeBufferBeforeNextToken, previousTokenIsPrintable, tokenIndentMap).toList
        }
      } else {
        alterSuspendFormatting(hiddenPredecessors(token).text) foreach { suspendFormatting = _ }
        if (suspendFormatting) {
          builder.append(hiddenPredecessors(token).rawText)
          tokenIndentMap += (token -> builder.currentColumn)
          builder.append(token.rawText)
        } else {
          val formattingInstruction = printableFormattingInstruction(previousTokenOption, token)
          val nextTokenUnindents = token.tokenType == RBRACE
          val includeBufferBeforeNextToken = true // <-- i.e. current token
          val hiddenTokens = hiddenPredecessors(token)
          val positionHintOption = if (hiddenTokens.isEmpty) Some(token.offset) else None
          edits :::= writeHiddenTokens(builder, hiddenTokens, formattingInstruction, nextTokenUnindents, includeBufferBeforeNextToken,
            previousTokenIsPrintable, tokenIndentMap, positionHintOption).toList
          tokenIndentMap += (token -> builder.currentColumn)
          val newTokenTextOpt: Option[String] = if (xmlRewrites contains token) Some(xmlRewrites(token)) else None
          edits :::= builder.write(token, newTokenTextOpt).toList
        }
      }
    }
    edits
      .reverse
      .flatMap { edit ⇒ if (edit.position >= offset) Some(edit.shift(-offset)) else None }
      .filter { case TextEdit(position, length, replacement) ⇒ s.substring(position, position + length) != replacement }
      .distinct
  }

  private def writeHiddenTokens(
    builder:                      StringBuilder,
    hiddenTokens:                 HiddenTokens,
    instruction:                  IntertokenFormatInstruction,
    nextTokenUnindents:           Boolean,
    includeBufferBeforeNextToken: Boolean,
    previousTokenIsPrintable:     Boolean,
    tokenIndentMap:               Map[Token, Int],
    positionHintOption:           Option[Int]                 = None
  ): Option[TextEdit] = {
    def writeIntertokenCompact() {
      val comments = hiddenTokens.comments
      for ((previousCommentOption, comment, nextCommentOption) ← Utils.withPreviousAndNext(comments)) {
        val needGapBetweenThisAndPrevious = cond(previousCommentOption) {
          case Some(MultiLineComment(_)) | Some(ScalaDocComment(_))      ⇒ true
          case _ if comment == comments.head && previousTokenIsPrintable ⇒ true
        }
        if (needGapBetweenThisAndPrevious)
          builder.append(" ")
        val extraIndent = comment match {
          case SingleLineComment(_) if nextCommentOption.isDefined || includeBufferBeforeNextToken ⇒ builder.currentIndent
          case _ ⇒ ""
        }
        builder.write(comment.token)
        builder.append(extraIndent)
      }
      val needGapBetweenThisAndFollowing = cond(comments.lastOption) {
        case Some(MultiLineComment(_)) if includeBufferBeforeNextToken ⇒ true
        case Some(ScalaDocComment(_)) if includeBufferBeforeNextToken  ⇒ true
      }
      if (needGapBetweenThisAndFollowing)
        builder.append(" ")
    }

    val startPos = builder.length
    val allWhitespace = hiddenTokens forall { _.isInstanceOf[Whitespace] }
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
      case PlaceAtColumn(indentLevel, spaces, relativeTo) ⇒
        require(!formattingPreferences(IndentWithTabs))
        writeIntertokenCompact()
        val relativeIndent = relativeTo flatMap tokenIndentMap.get getOrElse 0
        val indentLength = Spaces(formattingPreferences(IndentSpaces)).length(indentLevel)
        builder.append(" " * (indentLength + relativeIndent + spaces - builder.currentColumn))
      case EnsureNewlineAndIndent(indentLevel, relativeTo) ⇒
        require(!(formattingPreferences(IndentWithTabs) && relativeTo.isDefined))
        val baseIndentOption = relativeTo flatMap tokenIndentMap.get
        if (hiddenTokens.isEmpty) {
          builder.ensureAtBeginningOfLine()
          builder.indent(indentLevel, baseIndentOption)
        } else {
          val commentIndentLevel = if (nextTokenUnindents) indentLevel + 1 else indentLevel
          for ((previousOpt, hiddenToken, nextOpt) ← Utils.withPreviousAndNext(hiddenTokens)) {
            hiddenToken match {
              case ScalaDocComment(token) ⇒
                if (token.rawText.startsWith("/***")) {
                  builder.append(token.rawText)
                } else {
                  builder.ensureAtBeginningOfLine()
                  builder.indent(commentIndentLevel, baseIndentOption)
                  builder.append(formatScaladocComment(hiddenToken, commentIndentLevel))
                }
              case SingleLineComment(_) | MultiLineComment(_) ⇒
                if (builder.atBeginningOfLine)
                  builder.indent(commentIndentLevel, baseIndentOption)
                else if (builder.atVisibleCharacter) // Separation from previous visible token
                  builder.append(" ")
                builder.append(formatNonScaladocComment(hiddenToken, commentIndentLevel))
              case Whitespace(token) ⇒
                val newlineCount = token.text.count(_ == '\n')
                val newlinesToWrite = previousOpt match {
                  case Some(SingleLineComment(_)) ⇒ math.min(1, newlineCount)
                  case _                          ⇒ math.min(2, newlineCount)
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
                  if (previousOpt.exists(_.isInstanceOf[MultiLineComment]) && !token.text.contains('\n'))
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
      case Some(positionHint) if hiddenTokens.isEmpty ⇒
        Some(TextEdit(positionHint, length = 0, replacement = replacement))
      case _ ⇒
        for {
          firstToken ← hiddenTokens.firstTokenOption
          lastToken ← hiddenTokens.lastTokenOption
          start = firstToken.token.offset
          end = lastToken.token.lastCharacterOffset
          length = end - start + 1
        } yield TextEdit(start, length, replacement)
    }
  }

  class StringBuilderExtra(builder: StringBuilder) {

    def indent(indentLevel: Int, baseIndentOption: Option[Int] = None) = {
      for {
        baseIndent ← baseIndentOption
        n ← 1 to baseIndent
      } builder.append(" ")
      val indentChars = formattingPreferences.indentStyle.indent(indentLevel)
      builder.append(indentChars)
      builder
    }

    def write(token: Token, replacementOption: Option[String] = None): Option[TextEdit] = {
      val rewriteArrows = formattingPreferences(RewriteArrowSymbols)
      val actualReplacementOption = replacementOption orElse (condOpt(token.tokenType) {
        case ARROW if rewriteArrows  ⇒ "⇒"
        case LARROW if rewriteArrows ⇒ "←"
        case EOF                     ⇒ ""
      })
      builder.append(actualReplacementOption getOrElse token.rawText)
      actualReplacementOption map { replaceEdit(token, _) }
    }

    def write(hiddenToken: HiddenToken) = {
      builder.append(hiddenToken.token.rawText)
      builder
    }

    def newline() = {
      builder.append(newlineSequence)
      builder
    }

    def atBeginningOfLine = builder.isEmpty || lastChar == '\n'

    private def lastChar = builder(builder.length - 1)

    def currentColumn = {
      var pos = builder.length - 1
      while (pos >= 0 && builder(pos) != '\n')
        pos -= 1
      builder.length - pos - 1
    }

    def currentIndent = {
      val lineStart = builder.length - currentColumn
      var pos = lineStart
      while (pos < builder.length && builder(pos).isWhitespace)
        pos += 1
      builder.substring(lineStart, pos)
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
    val previousTypeOption = previousTokenOption map { _.tokenType }
    val nextTypeOption = nextTokenOption map { _.tokenType }
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
    import scalariform.lexer.Chars.isOperatorPart
    val type1 = token1.tokenType
    val type2 = token2.tokenType
    if (type2 == EOF)
      return Compact
    if (type1 == LPAREN && type2 != RPAREN && formattingPreferences(SpaceInsideParentheses))
      return CompactEnsuringGap
    if (type1 != LPAREN && type2 == RPAREN && formattingPreferences(SpaceInsideParentheses))
      return CompactEnsuringGap
    if (type1 == LBRACKET && type2 != RBRACKET && formattingPreferences(SpaceInsideBrackets))
      return CompactEnsuringGap
    if (type1 != LBRACKET && type2 == RBRACKET && formattingPreferences(SpaceInsideBrackets))
      return CompactEnsuringGap
    val xmlPreviousExceptions = Set(LBRACE, LPAREN, NEWLINE, NEWLINES)
    if (type1 == TYPE && type2.isId)
      return CompactEnsuringGap
    if (type2 == XML_START_OPEN && !(xmlPreviousExceptions.contains(type1) || type1.isXml))
      return CompactEnsuringGap
    if (type1 == USCORE && type2.isId && type2 != STAR)
      return CompactEnsuringGap
    if (type2 == USCORE && type1.isId)
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
    if (type1.isId && type2 == LBRACE)
      return CompactEnsuringGap
    if (type1 == LBRACE && type2 == RBRACE)
      return Compact //CompactEnsuringGap
    if (type1 == RBRACE && type2 == LBRACE)
      return CompactEnsuringGap
    if (type1 == RPAREN && type2.isLiteral)
      return CompactEnsuringGap
    if (type1 == RPAREN && type2.isId)
      return CompactEnsuringGap
    if (type1.isLiteral && type2.isId)
      return CompactEnsuringGap
    if (type1.isId && type2.isLiteral)
      return CompactEnsuringGap
    if (ENSURE_SPACE_AFTER(type1))
      return CompactEnsuringGap
    if (ENSURE_SPACE_BEFORE(type2))
      return CompactEnsuringGap
    if (type1.isId && type2.isId)
      return CompactEnsuringGap
    val firstCharOfToken2 = token2.text.head
    if (formattingPreferences(SpacesWithinPatternBinders) && type1.isId && type2 == AT)
      return CompactEnsuringGap
    if (formattingPreferences(SpacesWithinPatternBinders) && type1 == AT)
      return CompactEnsuringGap
    if (Set(HASH, AT).contains(type1) && isOperatorPart(firstCharOfToken2))
      return CompactEnsuringGap
    val lastCharOfToken1 = token1.text.last
    val firstIsIdEndingWithOpChar = type1.isId && (lastCharOfToken1 == '_' || isOperatorPart(lastCharOfToken1))
    if (Set(HASH, COLON, AT).contains(type2) && firstIsIdEndingWithOpChar)
      return CompactEnsuringGap
    if (type2 == COLON && formattingPreferences(SpaceBeforeColon))
      return CompactEnsuringGap
    type1 match {
      case ARROW if type2 != RPAREN ⇒ return CompactEnsuringGap // TODO: Redundant? no test fails.
      case COMMA                    ⇒ return CompactEnsuringGap
      case _                        ⇒
    }
    type2 match {
      case IF if type1 != LPAREN    ⇒ return CompactEnsuringGap
      case ARROW if type1 != LPAREN ⇒ return CompactEnsuringGap
      case AT if type2.isId         ⇒ return CompactEnsuringGap
      case _                        ⇒
    }
    Compact
  }

  protected def containsNewline(tokens: List[Token]): Boolean =
    tokens exists { token ⇒
      require(token != null)
      require(token.text != null, token)
      token != tokens.head && hiddenPredecessors(token).containsNewline ||
        token.text.contains("\n") ||
        isInferredNewline(token) && inferredNewlines(token).containsNewline // TODO: Why would an inferred newline not contain newline?
    }

  protected def containsNewline(astNode: AstNode): Boolean = containsNewline(astNode.tokens)

}

object ScalaFormatter {

  // format: OFF
  val ENSURE_SPACE_AFTER = Set(
    ABSTRACT, CASE, CATCH, CLASS, DEF,
    DO, ELSE, EXTENDS, FINAL,
    FINALLY, FOR, FORSOME, IF, IMPLICIT,
    IMPORT, LAZY, MATCH, NEW,
    OBJECT, OVERRIDE, PACKAGE, PRIVATE, PROTECTED,
    RETURN, SEALED, /* SUPER, THIS, */
    THROW, TRAIT, TRY, /* TYPE ,*/
    VAL, VAR, WHILE, WITH, YIELD,
    /* USCORE, */ COLON, EQUALS, ARROW, LARROW, SUBTYPE, VIEWBOUND, SUPERTYPE, /* HASH, AT */
    LBRACE, SEMI)

  val ENSURE_SPACE_BEFORE = Set(
    ABSTRACT, CASE, CATCH, CLASS, DEF,
    /* DO, */  ELSE, EXTENDS, FINAL,
    FINALLY, /* FOR, */ FORSOME, /* IF, */ IMPLICIT,
    /* IMPORT, */ LAZY, MATCH, /* NEW, */
    OBJECT, OVERRIDE, /* PACKAGE, */ PRIVATE, PROTECTED,
    /* RETURN, */ SEALED, /* SUPER, THIS, */
    /* THROW, */ TRAIT, /* TRY, TYPE, */
    VAL, VAR, /* WHILE, */ WITH, YIELD,
    /* USCORE, COLON, */ EQUALS, /* ARROW, */ LARROW, SUBTYPE, VIEWBOUND, SUPERTYPE, /*, HASH, AT, */
    RBRACE)
  // format: ON

  @throws(classOf[ScalaParserException])
  def format(source: String, formattingPreferences: IFormattingPreferences = FormattingPreferences(), lineDelimiter: Option[String] = None,
             initialIndentLevel: Int = 0, scalaVersion: String = ScalaVersions.DEFAULT_VERSION): String = {
    val edits = formatAsEdits(source, formattingPreferences, lineDelimiter, initialIndentLevel, scalaVersion)
    TextEditProcessor.runEdits(source, edits)
  }

  @throws(classOf[ScalaParserException])
  def formatAsEdits(source: String, formattingPreferences: IFormattingPreferences = FormattingPreferences(), lineDelimiter: Option[String] = None,
                    initialIndentLevel: Int = 0, scalaVersion: String = ScalaVersions.DEFAULT_VERSION): List[TextEdit] = {
    val specificFormatter = new SpecificFormatter {

      type Result = CompilationUnit

      def parse(parser: ScalaParser) = parser.compilationUnitOrScript()

      def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState(indentLevel = initialIndentLevel))

    }
    val (edits, _) = specificFormatter.fullFormat(source, scalaVersion = scalaVersion)(formattingPreferences)
    edits
  }

}
