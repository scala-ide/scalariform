package scalariform.lexer

import scala.annotation._
import scala.collection.mutable.{ Queue, Stack }
import scala.math.min
import scala.xml.parsing.TokenTests
import scalariform.lexer.CharConstants.SU
import scalariform.lexer.ScalaLexer._
import scalariform.lexer.Tokens._
import scalariform.utils.Utils

abstract class Lexer(reader: UnicodeEscapeReader) extends TokenTests {

  protected val forgiveLexerErrors: Boolean

  // TODO: use this
  protected val tokenTextBuffer = new StringBuilder

  private var actualTokenTextOffset = 0
  private var actualTokenTextLength = 0

  protected var eof = false
  protected var builtToken: Token = _

  // Two queues maintained in parallel. Invariant: chQueue.length == unicodeEscapesQueue.length
  private val chQueue = new Queue[Char]
  private val unicodeEscapesQueue = new Queue[Option[String]]

  protected var lastCh: Char = SU

  protected val modeStack = new Stack[LexerMode]

  protected def isUnicodeEscape = unicodeEscapesQueue.last.isDefined

  protected def ch: Char = {
    if (chQueue.isEmpty)
      slurpOneChar()
    chQueue.head
  }

  protected def ch(offset: Int) = {
    val extra = offset + 1 - chQueue.size
    for (n ← 1 to extra)
      slurpOneChar()
    chQueue(offset)
  }

  private def slurpOneChar() {
    val (c, unicodeEscapeOfPreviousRead) = reader.read()
    chQueue.enqueue(c)
    if (reader.isEof)
      eof = true
    unicodeEscapesQueue.enqueue(unicodeEscapeOfPreviousRead)
  }

  protected def nextChar() {
    lastCh = ch
    val virtualChar = chQueue.dequeue()
    tokenTextBuffer.append(virtualChar)
    val delta = unicodeEscapesQueue.dequeue() match {
      case None    ⇒ 1
      case Some(s) ⇒ s.length
    }
    actualTokenTextLength += delta
  }

  protected def token(tokenType: TokenType) {
    val startIndex = actualTokenTextOffset
    val tokenLength = actualTokenTextLength
    require(tokenType == EOF || tokenLength > 0)
    val stopIndex = min(startIndex + tokenLength - 1, reader.s.length - 1) // min protects against overeager consumption past EOF in forgiving mode   
    val rawText = reader.s.substring(actualTokenTextOffset, stopIndex + 1)
    val text = tokenTextBuffer.toString
    builtToken = Token(tokenType, text, startIndex, rawText)
    tokenTextBuffer.clear()
    actualTokenTextOffset = stopIndex + 1
    actualTokenTextLength = 0
  }

  protected def lookaheadIs(s: String): Boolean = s.zipWithIndex forall { case (c, index) ⇒ ch(index) == c }

  protected def munch(s: String) {
    require(lookaheadIs(s))
    for (_ ← 1 to s.length)
      nextChar()
  }

  protected def switchToScalaModeAndFetchToken()

  protected def switchToXmlModeAndFetchToken()

}
