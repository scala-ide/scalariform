package scalariform.lexer

import scala.annotation._
import scalariform.lexer.CharConstants.SU
import scalariform.lexer.Chars._
import scalariform.lexer.Tokens._
import scalariform.utils.Utils
import scalariform._

/**
 * Lexer implementation for non-XML Scala
 */
private[lexer] trait ScalaOnlyLexer { self: ScalaLexer ⇒

  private var processingSymbol = false

  private var possibleInterpolationId = false

  protected def fetchScalaToken() {
    (ch: @switch) match {
      case ' ' | '\t' | '\n' | '\r' /* TODO: | FF */ ⇒
        nextChar()
        getWhitespaceRest()
      case 'A' | 'B' | 'C' | 'D' | 'E' |
        'F' | 'G' | 'H' | 'I' | 'J' |
        'K' | 'L' | 'M' | 'N' | 'O' |
        'P' | 'Q' | 'R' | 'S' | 'T' |
        'U' | 'V' | 'W' | 'X' | 'Y' |
        'Z' | '$' | '_' |
        'a' | 'b' | 'c' | 'd' | 'e' |
        'f' | 'g' | 'h' | 'i' | 'j' |
        'k' | 'l' | 'm' | 'n' | 'o' |
        'p' | 'q' | 'r' | 's' | 't' |
        'u' | 'v' | 'w' | 'x' | 'y' |
        'z' ⇒
        nextChar()
        try {
          possibleInterpolationId = true
          getIdentRest()
        } finally
          possibleInterpolationId = false
      case '<' ⇒
        lastCh match {
          case SU | ' ' | '\t' | '\n' | '{' | '(' | '>' if ch(1) != SU && ch(1) != ':' && (isNameStart(ch(1)) || ch(1) == '!' || ch(1) == '?') ⇒
            switchToXmlModeAndFetchToken()
          case _ ⇒
            nextChar()
            getOperatorRest()
        }
      case '~' | '!' | '@' | '#' | '%' |
        '^' | '*' | '+' | '-' | /*'<' | */
        '>' | '?' | ':' | '=' | '&' |
        '|' | '\\' ⇒
        nextChar()
        getOperatorRest()
      case '/' ⇒
        (ch(1): @switch) match {
          case '/' ⇒ getSingleLineComment()
          case '*' ⇒ getMultilineComment()
          case _   ⇒ nextChar(); getOperatorRest()
        }
      case '0' ⇒
        if (ch(1) == 'x' || ch(1) == 'X')
          getHexNumber()
        else {
          nextChar()
          getNumber(base = 8)
        }
      case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ⇒
        getNumber(base = 10)
      case '`' ⇒ getBackquotedIdent()
      case '"' ⇒
        if (ch(1) == '"' && ch(2) == '"')
          getMultiLineStringLit()
        else
          getStringLit()

      case '\'' ⇒
        nextChar()
        if (isIdentifierStart(ch))
          charLitOr(() => getIdentRest)
        else if (isOperatorPart(ch) && (ch != '\\'))
          charLitOr(() => getOperatorRest)
        else {
          getLitChar()
          if (ch == '\'' || forgiveErrors) {
            nextChar()
            token(CHARACTER_LITERAL)
          } else
            throw new ScalaLexerException("unclosed character literal")
        }

      case '.' ⇒
        nextChar()
        if ('0' <= ch && ch <= '9')
          getFraction()
        else
          token(DOT)
      case ';' ⇒
        nextChar(); token(SEMI)
      case ',' ⇒
        nextChar(); token(COMMA)
      case '(' ⇒
        nextChar(); token(LPAREN)
      case '{' ⇒
        nextChar(); token(LBRACE)
        scalaMode.nestBrace()
      case ')' ⇒
        nextChar(); token(RPAREN)
      case '}' ⇒
        nextChar(); token(RBRACE)
        val nestingLevel = scalaMode.unnestBrace()
        if (nestingLevel == 0 && !isRootMode)
          popMode() // Go back to XML or string interpolation
      case '[' ⇒
        nextChar(); token(LBRACKET)
      case ']' ⇒
        nextChar(); token(RBRACKET)
      case SU ⇒
        token(EOF)
      case _ ⇒
        if (ch == '\u21D2') {
          nextChar()
          token(ARROW)
        } else if (ch == '\u2190') {
          nextChar()
          token(LARROW)
        } else if (Character.isUnicodeIdentifierStart(ch)) {
          nextChar()
          getIdentRest()
        } else if (isSpecial(ch)) {
          nextChar()
          getOperatorRest()
        } else if (forgiveErrors) {
          nextChar()
          getWhitespaceRest()
        } else
          throw new ScalaLexerException("illegal character: " + ch)
    }
  }

  @tailrec
  private def getWhitespaceRest(): Unit = (ch: @switch) match {
    case ' ' | '\t' | '\n' | '\r' /* TODO: | FF */ ⇒
      nextChar()
      getWhitespaceRest()
    case _ ⇒
      token(WS)
  }

  private def getStringLit() {
    getStringLitOrBackquotedIdent(delimiter = '"', errorMsg = "unclosed string literal", tokenType = STRING_LITERAL)
  }

  private def getBackquotedIdent() {
    getStringLitOrBackquotedIdent(delimiter = '`', errorMsg = "unclosed quoted identifer", tokenType = VARID, errorMsgOnEmpty = Some("empty quoted identifier"))
  }

  private def getStringLitOrBackquotedIdent(delimiter: Char, errorMsg: String, tokenType: TokenType, errorMsgOnEmpty: Option[String] = None) {
    //    require(ch == delimiter)
    nextChar()
    @tailrec
    def scanForClosingQuotes(firstTime: Boolean): Unit = ch match {
      case _ if eof ⇒
        if (forgiveErrors) token(tokenType) else throw new ScalaLexerException(errorMsg)
      case '\r' | '\n' if !isUnicodeEscape ⇒
        if (forgiveErrors) token(tokenType) else throw new ScalaLexerException(errorMsg)
      case `delimiter` if firstTime && errorMsgOnEmpty.isDefined ⇒
        if (forgiveErrors) token(tokenType) else throw new ScalaLexerException(errorMsgOnEmpty.get)
      case `delimiter` ⇒
        nextChar()
        token(tokenType)
      case _ ⇒
        getLitChar()
        scanForClosingQuotes(firstTime = false)
    }
    scanForClosingQuotes(firstTime = true)
  }

  private def getLitChar() {
    if (ch == '\\') {
      nextChar()
      if ('0' <= ch && ch <= '7') {
        val leadch = ch
        nextChar()
        if ('0' <= ch && ch <= '7') {
          nextChar()
          if (leadch <= '3' && '0' <= ch && ch <= '7')
            nextChar()
        }
      } else (ch: @switch) match {
        case 'b' | 't' | 'n' | 'f' | 'r' | '"' | '\'' | '\\' ⇒ nextChar()
        case _ ⇒ if (forgiveErrors) nextChar() else throw new ScalaLexerException("invalid escape character")
      }
    } else
      nextChar()
  }

  private def getMultiLineStringLit() {
    munch("\"\"\"")

    @tailrec
    def scanForClosingTripleQuotes() {
      if (lookaheadIs("\"\"\"")) {
        munch("\"\"\"")
        while (ch == '\"') { nextChar() }
        token(STRING_LITERAL)
      } else if (eof) {
        if (forgiveErrors) token(STRING_LITERAL) else throw new ScalaLexerException("unclosed multi-line string literal")
      } else {
        nextChar()
        scanForClosingTripleQuotes()
      }
    }
    scanForClosingTripleQuotes()
  }

  @tailrec
  final protected def getStringPart(multiLine: Boolean) {
    if (ch == '"') {
      if (multiLine) {
        nextChar()
        if (isTripleQuote()) {
          token(STRING_LITERAL)
          popMode()
        } else
          getStringPart(multiLine)
      } else {
        nextChar()
        token(STRING_LITERAL)
        popMode()
      }
    } else if (ch == '$') {
      nextChar()
      if (ch == '$') {
        nextChar()
        getStringPart(multiLine)
      } else if (ch == '_') {
        token(STRING_PART)
        stringInterpolationMode.interpolationVariable = true
      } else if (ch == '{') {
        token(STRING_PART)
        switchToScalaMode()
      } else if (Character.isUnicodeIdentifierStart(ch)) {
        token(STRING_PART)
        stringInterpolationMode.interpolationVariable = true
      } else {
        if (forgiveErrors) {
          nextChar()
          getStringPart(multiLine)
        } else
          throw new ScalaLexerException("invalid string interpolation")
      }
    } else {
      val isUnclosedLiteral = !isUnicodeEscape && (ch == SU || (!multiLine && (ch == '\r' || ch == '\n')))
      if (isUnclosedLiteral) {
        if (forgiveErrors) {
          token(STRING_LITERAL)
          popMode()
        } else
          throw new ScalaLexerException(if (!multiLine) "unclosed string literal" else "unclosed multi-line string literal")
      } else {
        nextChar()
        getStringPart(multiLine)
      }
    }
  }

  private def isTripleQuote(): Boolean =
    if (ch == '"') {
      nextChar()
      if (ch == '"') {
        nextChar()
        while (ch == '"')
          nextChar()
        true
      } else
        false
    } else
      false

  private def getIdentRest(): Unit = (ch: @switch) match {
    case 'A' | 'B' | 'C' | 'D' | 'E' |
      'F' | 'G' | 'H' | 'I' | 'J' |
      'K' | 'L' | 'M' | 'N' | 'O' |
      'P' | 'Q' | 'R' | 'S' | 'T' |
      'U' | 'V' | 'W' | 'X' | 'Y' |
      'Z' | '$' |
      'a' | 'b' | 'c' | 'd' | 'e' |
      'f' | 'g' | 'h' | 'i' | 'j' |
      'k' | 'l' | 'm' | 'n' | 'o' |
      'p' | 'q' | 'r' | 's' | 't' |
      'u' | 'v' | 'w' | 'x' | 'y' |
      'z' |
      '0' | '1' | '2' | '3' | '4' |
      '5' | '6' | '7' | '8' | '9' ⇒
      nextChar()
      getIdentRest()
    case '_' ⇒
      nextChar()
      getIdentOrOperatorRest()
    case _ ⇒
      if (Character.isUnicodeIdentifierPart(ch) && ch != SU) {
        nextChar()
        getIdentRest()
      } else
        finishNamed()
  }

  private def getOperatorRest(): Unit = (ch: @switch) match {
    case '~' | '!' | '@' | '#' | '%' |
      '^' | '*' | '+' | '-' | '<' |
      '>' | '?' | ':' | '=' | '&' |
      '|' | '\\' ⇒
      nextChar(); getOperatorRest()
    case '/' ⇒
      (ch(1): @switch) match {
        case '/' | '*' ⇒ finishNamed()
        case _         ⇒ nextChar(); getOperatorRest()
      }
    case _ ⇒
      if (isSpecial(ch)) { nextChar(); getOperatorRest() } else finishNamed()
  }

  private def getIdentOrOperatorRest() {
    if (isIdentifierPart(ch))
      getIdentRest()
    else ch match {
      case '~' | '!' | '@' | '#' | '%' |
        '^' | '*' | '+' | '-' | '<' |
        '>' | '?' | ':' | '=' | '&' |
        '|' | '\\' | '/' ⇒
        getOperatorRest()
      case _ ⇒
        if (isSpecial(ch))
          getOperatorRest()
        else
          finishNamed()
    }
  }

  private def finishNamed() {
    val tokenType =
      if (processingSymbol)
        SYMBOL_LITERAL
      else if (possibleInterpolationId && ch == '\"' && scalaVersion >= ScalaVersions.Scala_2_10) {
        switchToStringInterpolationMode(lookaheadIs("\"\"\""))
        INTERPOLATION_ID
      } else
        Keywords(getTokenText).getOrElse(VARID)

    token(tokenType)
  }

  private def getSingleLineComment() {
    //    require(ch == '/')
    nextChar()
    //    require(ch == '/')
    nextChar()

    @tailrec
    def consumeUntilNewline(): Unit = (ch: @switch) match {
      case '\n' ⇒
        nextChar()
        token(LINE_COMMENT)
      case '\r' if ch(1) != '\n' ⇒
        nextChar()
        token(LINE_COMMENT)
      case SU if eof ⇒
        token(LINE_COMMENT)
      case _ ⇒
        nextChar()
        consumeUntilNewline()
    }

    consumeUntilNewline()
  }

  private def getMultilineComment() {
    munch("/*")

    @tailrec
    def consumeUntilSplatSlash(nesting: Int) {
      if (nesting == 0)
        token(MULTILINE_COMMENT)
      else
        (ch: @switch) match {
          case '*' if (ch(1) == '/') ⇒
            nextChar()
            nextChar()
            consumeUntilSplatSlash(nesting - 1)
          case '/' if (ch(1) == '*') ⇒
            nextChar()
            nextChar()
            consumeUntilSplatSlash(nesting + 1)
          case SU if eof ⇒
            if (forgiveErrors) token(MULTILINE_COMMENT) else throw new ScalaLexerException("Unterminated comment")
          case _ ⇒
            nextChar()
            consumeUntilSplatSlash(nesting)
        }
    }

    consumeUntilSplatSlash(nesting = 1)
  }

  private def getFraction() {
    while ('0' <= ch && ch <= '9') { nextChar() }
    if (ch == 'e' || ch == 'E') {
      nextChar()
      if (ch == '+' || ch == '-')
        nextChar()
      while ('0' <= ch && ch <= '9') { nextChar() }
    }
    if (ch == 'd' || ch == 'D' || ch == 'f' || ch == 'F')
      nextChar()
    checkNoLetter()
    token(FLOATING_POINT_LITERAL)
  }

  private def getHexNumber() {
    //    require(ch == '0')
    nextChar()
    //    require(ch == 'x' || ch == 'X')
    nextChar()
    @tailrec
    def munchHexDigits(): Unit = (ch: @switch) match {
      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' ⇒
        nextChar(); munchHexDigits()
      case 'l' | 'L' ⇒
        nextChar(); token(INTEGER_LITERAL)
      case _ ⇒
        token(INTEGER_LITERAL); checkNoLetter()
    }
    munchHexDigits()
  }

  private def getNumber(base: Int) {
    def isDigit(c: Char) = if (c == SU) false else (Character isDigit c)
    val base1 = if (base < 10) 10 else base

    // read 8,9's even if format is octal, produce a malformed number error afterwards.
    while (Utils.digit2int(ch, base1) >= 0)
      nextChar()

    def restOfUncertainToken() = {
      def isEfd = ch match {
        case 'e' | 'E' | 'f' | 'F' | 'd' | 'D' ⇒ true
        case _                                 ⇒ false
      }
      def isL = ch match {
        case 'l' | 'L' ⇒ true
        case _         ⇒ false
      }

      if (isEfd)
        getFraction()
      else {
        if (isL)
          nextChar()
        else
          checkNoLetter()
        token(INTEGER_LITERAL)
      }
    }

    if (ch == '.') {
      val c = ch(1)

      if (scalaVersion >= ScalaVersions.Scala_2_11 && !isDigit(c)) {
        token(INTEGER_LITERAL)
        return
      }

      val isDefinitelyNumber =
        (c: @switch) match {
          /* Another digit is a giveaway. */
          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ⇒
            true

          /* Backquoted idents like 22.`foo`. */
          case '`' ⇒
            token(INTEGER_LITERAL); return
          /* Note the early return */

          /* These letters may be part of a literal, or a method invocation on an Int */
          case 'd' | 'D' | 'f' | 'F' ⇒
            !isIdentifierPart(ch(2))

          /* A little more special handling for e.g. 5e7 */
          case 'e' | 'E' ⇒
            val ch2 = ch(2)
            !isIdentifierPart(ch2) || (isDigit(ch2) || ch2 == '+' || ch2 == '-')

          case x ⇒
            !isIdentifierStart(x)
        }

      if (isDefinitelyNumber) {
        nextChar()
        getFraction()
      } else
        restOfUncertainToken()
    } else
      restOfUncertainToken()

  }

  private def checkNoLetter() {
    if (isIdentifierPart(ch) && ch >= ' ' && !forgiveErrors)
      throw new ScalaLexerException("Invalid literal number: " + ch)
  }

  private def charLitOr(op: () ⇒ Unit) {
    nextChar()
    if (ch == '\'') {
      nextChar()
      token(CHARACTER_LITERAL)
    } else {
      processingSymbol = true
      op()
      processingSymbol = false
    }
  }

}
