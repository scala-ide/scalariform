package scalariform.lexer

import java.io.Reader

import scala.collection.mutable.Queue

class UnicodeEscapeReader(delegate: Reader) extends Reader {
  import ScalaLexer._

  private var chQueue = new Queue[Int]
  private var consecutiveBackslashCount = 0
  var unicodeEscapeOfPreviousRead: Option[String] = None

  private def readFromQueue(): Int = {
    if (chQueue.isEmpty)
      chQueue.enqueue(delegate.read())
    chQueue.dequeue()
  }

  private def lookahead(offset: Int) = {
    val extra = offset + 1 - chQueue.size
    for (n ← 1 to extra)
      chQueue.enqueue(delegate.read())
    chQueue(offset)
  }

  override def read(): Int = {
    val ch = readFromQueue()
    if (ch == '\\') {
      if (lookahead(0) == 'u' && consecutiveBackslashCount % 2 == 0) {
        val sb = new StringBuilder
        sb.append('\\')
        while (lookahead(0) == 'u') {
          readFromQueue()
          sb.append('u')
        }
        val digitCh1 = readFromQueue(); sb.append(digitCh1.asInstanceOf[Char])
        val digitCh2 = readFromQueue(); sb.append(digitCh2.asInstanceOf[Char])
        val digitCh3 = readFromQueue(); sb.append(digitCh3.asInstanceOf[Char])
        val digitCh4 = readFromQueue(); sb.append(digitCh4.asInstanceOf[Char])
        val digit1 = ScalaLexer.digit2int(digitCh1, base = 16)
        val digit2 = ScalaLexer.digit2int(digitCh2, base = 16)
        val digit3 = ScalaLexer.digit2int(digitCh3, base = 16)
        val digit4 = ScalaLexer.digit2int(digitCh4, base = 16)
        if (digit1 < 0 || digit2 < 0 || digit3 < 0 || digit4 < 0) throw new ScalaLexerException("error in unicode escape")
        val code = digit1 << 12 | digit2 << 8 | digit3 << 4 | digit4
        unicodeEscapeOfPreviousRead = Some(sb.toString)
        consecutiveBackslashCount = 0
        code.toChar
      } else {
        unicodeEscapeOfPreviousRead = None
        consecutiveBackslashCount += 1
        ch
      }
    } else {
      unicodeEscapeOfPreviousRead = None
      consecutiveBackslashCount = 0
      ch
    }
  }

  def read(cbuf: Array[Char], off: Int, len: Int): Int = throw new UnsupportedOperationException()

  def close() { delegate.close() }

}