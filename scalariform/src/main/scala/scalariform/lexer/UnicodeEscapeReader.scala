package scalariform.lexer

import CharConstants._
import ScalaLexer._

class UnicodeEscapeReader(val s: String) {

  private var pos: Int = 0
  private var consecutiveBackslashCount = 0
  var unicodeEscapeOfPreviousRead: Option[String] = None

  private def safeGet(pos: Int) = if (pos >= s.length) SU else s charAt pos

  def getPos = pos

  private var eof = s.isEmpty

  def isEof = eof

  private def consumeNextCharacter(): Char = {
    val result = safeGet(pos)
    if (pos >= s.length)
      eof = true
    pos += 1
    result
  }

  private def lookahead(offset: Int) = safeGet(pos + offset)

  def read(): Char = {
    val ch = consumeNextCharacter()
    if (ch == '\\')
      if (lookahead(0) == 'u' && consecutiveBackslashCount % 2 == 0)
        getUnicodeChar()
      else {
        unicodeEscapeOfPreviousRead = None
        consecutiveBackslashCount += 1
        ch
      }
    else {
      unicodeEscapeOfPreviousRead = None
      consecutiveBackslashCount = 0
      ch
    }
  }

  private def getUnicodeChar(): Char = {
    val sb = new StringBuilder

    sb.append('\\')

    while (lookahead(0) == 'u') {
      pos += 1
      sb.append('u')
    }

    val digitCh1 = consumeNextCharacter()
    sb.append(digitCh1)
    val digit1 = digit2int(digitCh1, base = 16)

    val digitCh2 = consumeNextCharacter()
    sb.append(digitCh2)
    val digit2 = digit2int(digitCh2, base = 16)

    val digitCh3 = consumeNextCharacter()
    sb.append(digitCh3)
    val digit3 = digit2int(digitCh3, base = 16)

    val digitCh4 = consumeNextCharacter()
    sb.append(digitCh4)
    val digit4 = digit2int(digitCh4, base = 16)

    if (digit1 < 0 || digit2 < 0 || digit3 < 0 || digit4 < 0)
      throw new ScalaLexerException("error in unicode escape: " + sb.toString)

    val decodedChar = (digit1 << 12 | digit2 << 8 | digit3 << 4 | digit4).toChar

    unicodeEscapeOfPreviousRead = Some(sb.toString)
    consecutiveBackslashCount = 0
    decodedChar
  }

}
