package scalariform.lexer

import scalariform.lexer.CharConstants.SU
import scalariform.utils.Utils._

object UnicodeEscapeDecoder {

  /**
   * Decode unicode escapes of the form "\u0061" in the given text.
   * If forgiveErrors is true, then no exception will be thrown on malformed escapes.
   */
  @throws(classOf[ScalaLexerException])
  def decode(text: String, forgiveErrors: Boolean = false): String =
    new UnicodeEscapeReader(text, forgiveErrors).mkString

}

trait IUnicodeEscapeReader extends Iterator[Char] {

  val text: String

  /**
   * @return true if all the available characters have been read.
   */
  def isEof: Boolean

  /**
   * @return the next character from the post-decoded text
   */
  @throws(classOf[ScalaLexerException])
  def read(): Char

  /**
   * @return the corresponding unicode escape sequence if the last character read was decoded, otherwise None.
   */
  def unicodeEscapeOpt: Option[String]

  def next() = read()

  def hasNext = !isEof

  /**
   * Return a clone of this reader initialised to the current state
   */
  def copy: IUnicodeEscapeReader

}

class UnicodeEscapeReader(val text: String, forgiveErrors: Boolean = false) extends IUnicodeEscapeReader {

  private var pos: Int = 0

  private var unicodeEscapeSequence: String = null

  /**
   * To distinguish cases like "\\u" from unicode escape sequences.
   */
  private var consecutiveBackslashCount = 0

  def copy: UnicodeEscapeReader = {
    val reader = new UnicodeEscapeReader(text, forgiveErrors)
    reader.pos = pos
    reader.unicodeEscapeSequence = unicodeEscapeSequence
    reader.consecutiveBackslashCount = consecutiveBackslashCount
    reader
  }

  def isEof = pos >= text.length

  @throws(classOf[ScalaLexerException])
  def read(): Char = {
    val ch = consumeNextCharacter()
    unicodeEscapeSequence = null
    if (ch == '\\')
      if (nextChar == 'u' && consecutiveBackslashCount % 2 == 0) {
        consecutiveBackslashCount = 0
        readUnicodeChar(pos - 1)
      } else {
        consecutiveBackslashCount += 1
        ch
      }
    else {
      consecutiveBackslashCount = 0
      ch
    }
  }

  def unicodeEscapeOpt: Option[String] = Option(unicodeEscapeSequence)

  private def consumeNextCharacter(): Char = {
    val result = safeGet(pos)
    pos += 1
    result
  }

  private def nextChar = safeGet(pos)

  private def safeGet(pos: Int): Char = if (pos >= text.length) SU else text(pos)

  private def readUnicodeChar(startPos: Int): Char = {
    this.unicodeEscapeSequence = consumeUnicodeEscape()
    val decodedChar = decodeUnicodeChar(unicodeEscapeSequence takeRight 4 toList, unicodeEscapeSequence, startPos)
    decodedChar
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

  private def decodeUnicodeChar(digits: List[Char], unicodeEscapeSequence: String, startPos: Int): Char = {
    val List(digit1, digit2, digit3, digit4) = digits.map(digit2int(_, base = 16))
    if (digit1 < 0 || digit2 < 0 || digit3 < 0 || digit4 < 0)
      if (forgiveErrors)
        ' '
      else {
        val (line, column) = lineAndColumn(startPos)
        throw new ScalaLexerException("[" + line + ":" + column + "] error in unicode escape: '" + unicodeEscapeSequence + "'")
      }
    else
      (digit1 << 12 | digit2 << 8 | digit3 << 4 | digit4).toChar
  }

  private def lineAndColumn(offset: Int): (Int, Int) = {
    var line = 1
    var column = 1
    for (i ← 0 until offset) {
      if (text(i) == '\n') {
        line += 1
        column = 1
      } else
        column += 1
    }
    (line, column)
  }

}

class NoUnicodeEscapeReader(val text: String) extends IUnicodeEscapeReader {

  private var pos = 0

  def copy = {
    val reader = new NoUnicodeEscapeReader(text)
    reader.pos = pos
    reader
  }

  def isEof: Boolean = pos >= text.length

  def read(): Char = {
    val result = if (isEof) SU else text(pos)
    pos += 1
    result
  }

  def unicodeEscapeOpt: Option[String] = None

}
