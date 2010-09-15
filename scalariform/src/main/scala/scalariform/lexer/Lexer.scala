package scalariform.lexer

import scala.annotation.{ switch, tailrec }
import scalariform.lexer.Tokens._
import scalariform.utils.Utils
import scala.xml.parsing.TokenTests
import scala.collection.mutable.{ Queue, Stack }
import ScalaLexer._

object CharConstants {
  final val SU = '\u001A'
}

abstract class Lexer(reader: UnicodeEscapeReader) extends TokenTests {
  import CharConstants._

  protected val tokenTextBuffer = new StringBuilder

  private var actualTokenTextOffset = 0
  private var actualTokenTextLength = 0

  protected var eof = false
  protected var builtToken: Option[Token] = None

  // Two queues maintained in parallel. Invariant: chQueue.length == unicodeEscapesQueue.length
  private val chQueue = new Queue[Char]
  private val unicodeEscapesQueue = new Queue[Option[String]]

  protected var lastCh: Char = SU

  protected val modeStack = new Stack[LexerMode]

  trait LexerMode

  protected def isUnicodeEscape = reader.unicodeEscapeOfPreviousRead.isDefined

  // TODO: Merge with ch(offset)
  protected def ch: Char = {
    if (chQueue.isEmpty) {
      val c = reader.read()
      if (reader.isEof)
        eof = true
      chQueue.enqueue(c)
      unicodeEscapesQueue.enqueue(reader.unicodeEscapeOfPreviousRead)
    }
    chQueue.head
  }

  protected def ch(offset: Int) = {
    val extra = offset + 1 - chQueue.size
    for (n ← 1 to extra) {
      chQueue.enqueue(reader.read())
      unicodeEscapesQueue.enqueue(reader.unicodeEscapeOfPreviousRead)
    }
    chQueue(offset)
  }

  protected def nextChar() {
    lastCh = ch
    val virtualChar = chQueue.dequeue()
    tokenTextBuffer.append(virtualChar)
    val delta = unicodeEscapesQueue.dequeue() match {
      case None ⇒ 1
      case Some(s) ⇒ s.length
    }
    actualTokenTextLength += delta
  }

  protected def token(tokenType: TokenType) {
    val startIndex = actualTokenTextOffset
    val tokenLength = actualTokenTextLength
    require(tokenType == EOF || tokenLength > 0)
    val stopIndex = startIndex + tokenLength - 1
    val tokenText = reader.s.substring(actualTokenTextOffset, stopIndex + 1)
    val token = new Token(tokenType, tokenText, startIndex, stopIndex)
    builtToken = Some(token)
    tokenTextBuffer.clear()
    actualTokenTextOffset = stopIndex + 1
    actualTokenTextLength = 0
    // println("Token: " + token)
  }

  protected def lookaheadIs(s: String): Boolean = Utils.enumerate(s) forall { case (index, c) ⇒ ch(index) == c }

  protected def munch(s: String) {
    require(lookaheadIs(s))
    for (_ ← 1 to s.length)
      nextChar()
  }

  protected def switchToScalaModeAndFetchToken()

  protected def switchToXmlModeAndFetchToken()

}
