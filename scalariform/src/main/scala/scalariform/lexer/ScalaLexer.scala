package scalariform.lexer
import scala.annotation.{ switch, tailrec }
import scalariform.lexer.Tokens._
import scalariform.utils.Utils

import scala.collection.mutable.{ Queue, Stack, ListBuffer }

class ScalaLexer(reader: UnicodeEscapeReader) extends Lexer(reader) with ScalaOnlyLexer with XmlLexer {

  modeStack.push(new ScalaMode())

  def nextToken(): Token = {
    if (eof)
      super[Lexer].token(EOF)
    else if (isXmlMode)
      fetchXmlToken()
    else
      fetchScalaToken()
    val token = builtToken.get
    builtToken = None
    token
  }

  protected def switchToScalaModeAndFetchToken() {
    modeStack.push(new ScalaMode())
    fetchScalaToken()
  }

  protected def switchToXmlModeAndFetchToken(): Unit = {
    modeStack.push(new XmlMode())
    fetchXmlToken()
  }

}

object ScalaLexer {
  import java.io._

  private[lexer] def digit2int(ch: Int, base: Int): Int = {
    if ('0' <= ch && ch <= '9' && ch < '0' + base)
      ch - '0'
    else if ('A' <= ch && ch < 'A' + base - 10)
      ch - 'A' + 10
    else if ('a' <= ch && ch < 'a' + base - 10)
      ch - 'a' + 10
    else
      -1
  }

  def tokeniseFull(file: File): (NewlineInferencer, List[Token]) = {
    val s = scala.io.Source.fromFile(file).mkString
    tokeniseFull(s)
  }

  def tokeniseFull(s: String): (NewlineInferencer, List[Token]) = {
    val lexer = new NewlineInferencer(new WhitespaceAndCommentsGrouper(new ScalaLexer(new UnicodeEscapeReader(s))))
    val tokenBuffer = new ListBuffer[Token]
    var continue = true
    while (continue) {
      val token = lexer.nextToken()
      tokenBuffer += token
      if (token.getType == Tokens.EOF)
        continue = false
    }

    (lexer, tokenBuffer.toList)
  }

  def tokenise(s: String): List[Token] = tokeniseFull(s)._2

  def rawTokenise(s: String): List[Token] = {
    val lexer = new ScalaLexer(new UnicodeEscapeReader(s))
    var actualTokens: List[Token] = Nil
    var continue = true
    while (continue) {
      val token = lexer.nextToken()
      actualTokens ::= token
      if (token.getType == Tokens.EOF)
        continue = false
    }
    (actualTokens.tail).reverse
  }

  def rawTokenise2(s: String): List[Token] = {
    val lexer = new WhitespaceAndCommentsGrouper(new ScalaLexer(new UnicodeEscapeReader(s)))
    var actualTokens: List[Token] = Nil
    var continue = true
    while (lexer.hasNext) {
      val (_, token) = lexer.next()
      actualTokens ::= token
      if (token.getType == Tokens.EOF)
        continue = false
    }
    (actualTokens.tail).reverse
  }

}
