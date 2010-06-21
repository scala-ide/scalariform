package scalariform.lexer
import scala.annotation.{ switch, tailrec }
import scalariform.lexer.Tokens._
import scalariform.utils.Utils

import scala.collection.mutable.{ Queue, Stack }

object CharConstants {
  final val SU = '\u001A' // TODO: Use as EOF
  final val EOF_CHAR = -1
}

abstract class Lexer(reader: UnicodeEscapeReader) extends scala.xml.parsing.TokenTests {
  import ScalaLexer._
  import CharConstants._

  protected val tokenTextBuffer = new StringBuilder
  private val actualTokenTextBuffer = new StringBuilder

  protected var eof = false
  protected var builtToken: Option[Token] = None

  private val chQueue = new Queue[Int]
  private val unicodeEscapesQueue = new Queue[Option[String]]
  protected var lastCh = -1

  var tokenStart: Int = 0

  protected val modeStack = new Stack[LexerMode]

  trait LexerMode

  protected def isUnicodeEscape = reader.unicodeEscapeOfPreviousRead.isDefined

  // TODO: Merge with ch(offset)
  protected def ch: Int = {
    if (chQueue.isEmpty) {
      val c = reader.read()
      if (c == EOF_CHAR)
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
    val virtualChar = chQueue.dequeue().asInstanceOf[Char]
    tokenTextBuffer.append(virtualChar)
    unicodeEscapesQueue.dequeue() match {
      case None ⇒ actualTokenTextBuffer.append(virtualChar)
      case Some(s) ⇒ actualTokenTextBuffer.append(s)
    }
  }

  protected def token(tokenType: TokenType) {
    val tokenText = actualTokenTextBuffer.toString
    val tokenLength = tokenText.length
    require(tokenType == EOF || tokenLength > 0)
    val startIndex = tokenStart
    val stopIndex = tokenStart + tokenLength - 1
    val token = new Token(tokenType, tokenText, startIndex, stopIndex)
    tokenStart = tokenStart + tokenLength
    builtToken = Some(token)
    tokenTextBuffer.clear()
    actualTokenTextBuffer.clear()
    // println("Token: " + token)
  }

  protected def lookaheadIs(s: String): Boolean = Utils.enumerate(s) forall { case (index, c) ⇒ ch(index) == c }
  protected def munch(s: String) {
    require(lookaheadIs(s))
    for (_ ← 1 to s.length)
      nextChar()
  }

  protected def switchToScalaModeAndFetchToken(): Unit
  protected def switchToXmlModeAndFetchToken(): Unit

}
