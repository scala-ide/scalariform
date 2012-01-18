package scalariform.lexer

import java.io.File
import scala.annotation._
import scala.collection.mutable.{ Queue, ListBuffer }
import scala.io.Source
import scala.math.min
import scala.xml.parsing.TokenTests
import scalariform.lexer.CharConstants.SU
import scalariform.lexer.Tokens._
import scalariform.utils.Utils
import scalariform._

class ScalaLexer(
  reader: IUnicodeEscapeReader,
  protected val forgiveErrors: Boolean = false,
  protected val scalaVersion: ScalaVersionGroup = SCALA_28_29_210)
    extends ScalaOnlyLexer with XmlLexer with ModeStack with TokenTests with Iterator[Token] {

  import ScalaLexer._

  /**
   * Circular buffer of characters yet to be processed (after unicode escaping)
   */
  private val charBuffer: Array[Char] = Array.fill(BUFFER_SIZE)(SU)

  /**
   * A circular buffer of the unicode escape, if any, associated with the corresponding character in charBuffer.
   */
  private val unicodeEscapesBuffer: Array[Option[String]] = Array.fill(BUFFER_SIZE)(None)

  private var bufferStart = 0

  private var bufferEnd = 0

  private var seenUnicodeEscape = false

  /**
   * Start position of this token in the (pre Unicode escaped) text
   */
  private var tokenOffset = 0

  /**
   * Length so far of this token (before unicode escaping)
   */
  private var tokenLength = 0

  /**
   * Number of characters left in queues before end of file, or -1, if this is unknown
   */
  private var untilEof = -1

  /**
   * The previous character
   */
  protected var lastCh: Char = SU

  private var tokenText: String = _

  private var rawText: String = _

  private var stopIndex: Int = 0

  private var builtToken: Token = _

  protected def eof = untilEof == 0

  private def charsInBuffer = (BUFFER_SIZE + bufferEnd - bufferStart) & BUFFER_MASK

  /**
   * Is the current character the result of a unicode escape?
   */
  protected def isUnicodeEscape = unicodeEscapesBuffer(bufferStart).isDefined

  /**
   * Get the current character.
   */
  protected def ch: Char = {
    if (bufferEnd == bufferStart)
      bufferOneCharacter()
    charBuffer(bufferStart)
  }

  /**
   * Get the character at the given lookahead from the current position.
   */
  protected def ch(lookahead: Int) = {
    for (n ← 1 to lookahead + 1 - charsInBuffer)
      bufferOneCharacter()
    charBuffer((bufferStart + lookahead) & BUFFER_MASK)
  }

  private def bufferOneCharacter() {
    charBuffer(bufferEnd) = reader.read()
    unicodeEscapesBuffer(bufferEnd) = reader.unicodeEscapeOpt
    bufferEnd = (bufferEnd + 1) & BUFFER_MASK
    if (untilEof == -1 && reader.isEof)
      untilEof = charsInBuffer
  }

  /**
   * Accept the current character and advance to the next.
   */
  protected def nextChar() {
    if (bufferEnd == bufferStart)
      bufferOneCharacter()
    lastCh = charBuffer(bufferStart)
    val unicodeEscapeOpt = unicodeEscapesBuffer(bufferStart)
    bufferStart = (bufferStart + 1) & BUFFER_MASK

    tokenLength +=
      (unicodeEscapeOpt match {
        case None    ⇒ 1
        case Some(s) ⇒ s.length
      })
    seenUnicodeEscape |= unicodeEscapeOpt.isDefined

    if (untilEof > 0)
      untilEof -= 1
  }

  // For debug
  private def bufferContents: String =
    (for (n ← 0 until charsInBuffer) yield charBuffer((bufferStart + n) & BUFFER_MASK)).mkString

  /**
   * Mark the end of a token of the given type.
   */
  protected def token(tokenType: TokenType) {
    //    require(tokenType == EOF || tokenLength > 0)
    finaliseTokenData()
    builtToken = Token(tokenType, tokenText, tokenOffset, rawText)

    if (seenUnicodeEscape)
      builtToken.containsUnicodeEscape = true
    resetTokenData()
  }

  private def resetTokenData() {
    rawText = null
    tokenText = null
    tokenOffset = stopIndex + 1
    tokenLength = 0
    seenUnicodeEscape = false
  }

  private def finaliseTokenData() {
    if (tokenText == null) {
      stopIndex = math.min(tokenOffset + tokenLength - 1, reader.text.length - 1) // min protects against overeager consumption past EOF
      rawText = reader.text.substring(tokenOffset, stopIndex + 1)
      tokenText =
        if (seenUnicodeEscape)
          UnicodeEscapeDecoder.decode(rawText, forgiveErrors)
        else
          rawText
    }
  }

  private[lexer] def text = reader.text

  protected def getTokenText: String = {
    finaliseTokenData()
    tokenText
  }

  protected def lookaheadIs(s: String): Boolean =
    s.zipWithIndex forall { case (c, index) ⇒ ch(index) == c }

  protected def munch(s: String) {
    //    require(lookaheadIs(s))
    for (_ ← 1 to s.length)
      nextChar()
  }

  def next(): Token = {
    if (isXmlMode)
      fetchXmlToken()
    else
      fetchScalaToken()
    builtToken
  }

  def hasNext = !eof

}

object ScalaLexer {

  private val BUFFER_SIZE = 16 // sufficient lookahead for "</xml:unparsed>" (15 chars)

  private val BUFFER_MASK = BUFFER_SIZE - 1

  def createRawLexer(s: String, forgiveErrors: Boolean = false, scalaVersion: ScalaVersionGroup = SCALA_28_29_210): ScalaLexer =
    new ScalaLexer(new UnicodeEscapeReader(s, forgiveErrors), forgiveErrors, scalaVersion)

  def tokenise(file: File): List[Token] = tokenise(Source.fromFile(file).mkString)

  def tokenise(s: String, forgiveErrors: Boolean = false): List[Token] = {
    val lexer = new NewlineInferencer(new WhitespaceAndCommentsGrouper(createRawLexer(s, forgiveErrors)))
    lexer.toList
  }

  /**
   * @param forgiveErrors -- if true, no exceptions will be thrown
   * @return a list of tokens from the source, including whitespace and comment tokens. No NEWLINE or
   * NEWLINES tokens are inferred. The final token will be of type EOF.
   */
  @throws(classOf[ScalaLexerException])
  def rawTokenise(s: String, forgiveErrors: Boolean = false, scalaVersion: ScalaVersionGroup = SCALA_28_29_210): List[Token] =
    createRawLexer(s, forgiveErrors, scalaVersion).toList

}
