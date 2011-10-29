package scalariform.lexer

import scala.xml.Utility.SU
import scalariform.lexer.ScalaLexer._

class UnicodeEscapeReader(val s: String, forgiveLexerErrors: Boolean = false) {

  private var pos: Int = 0

  private var eof = s == ""

  /**
   * To distinguish cases like "\\u" from unicode escape sequences.
   */
  private var consecutiveBackslashCount = 0

  /**
   * @return the next logical character paired with the unicode escape sequence that encoded it, if any.
   */
  @throws(classOf[ScalaLexerException])
  def read(): (Char, Option[String]) = {
    val ch = consumeNextCharacter()
    if (ch == '\\')
      if (nextChar == 'u' && consecutiveBackslashCount % 2 == 0) {
        consecutiveBackslashCount = 0
        readUnicodeChar()
      } else {
        consecutiveBackslashCount += 1
        (ch, None)
      }
    else {
      consecutiveBackslashCount = 0
      (ch, None)
    }
  }

  private def readUnicodeChar(): (Char, Option[String]) = {
    val unicodeEscapeSequence = consumeUnicodeEscape()
    val decodedChar = decodeUnicodeChar(unicodeEscapeSequence takeRight 4 toList, unicodeEscapeSequence)
    (decodedChar, Some(unicodeEscapeSequence))
  }

  private def consumeUnicodeEscape(): String = {
    val sb = new StringBuilder
    sb.append('\\')

    // Repeating u's are allowed in Unicode escapes (bizarrely enough):
    do sb.append(consumeNextCharacter())
    while (nextChar == 'u')

    for (n ← 1 to 4)
      sb.append(consumeNextCharacter())

    sb.toString
  }

  private def decodeUnicodeChar(digits: List[Char], unicodeEscapeSequence: String): Char = {
    val List(digit1, digit2, digit3, digit4) = digits.map(digit2int(_, base = 16))
    if (digit1 < 0 || digit2 < 0 || digit3 < 0 || digit4 < 0)
      if (forgiveLexerErrors) ' ' else throw new ScalaLexerException("Error in unicode escape: " + unicodeEscapeSequence)
    else
      (digit1 << 12 | digit2 << 8 | digit3 << 4 | digit4).toChar
  }

  private def consumeNextCharacter(): Char = {
    val result = safeGet(pos)
    if (pos >= s.length)
      eof = true
    pos += 1
    result
  }

  private def nextChar = safeGet(pos)

  private def safeGet(pos: Int): Char = if (pos >= s.length) SU else s(pos)

  def isEof = eof

}