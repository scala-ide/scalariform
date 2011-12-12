package scalariform.lexer

import java.io.File
import scala.annotation._
import scala.collection.mutable.{ Queue, Stack, ListBuffer }
import scala.io.Source
import scalariform.lexer.Tokens._
import scalariform.utils.Utils

class ScalaLexer(reader: UnicodeEscapeReader, forgiveErrors: Boolean = false)
    extends Lexer(reader) with ScalaOnlyLexer with XmlLexer {

  override protected val forgiveLexerErrors = forgiveErrors

  modeStack.push(new ScalaMode)

  def nextToken(): Token = {
    if (isXmlMode)
      fetchXmlToken()
    else
      fetchScalaToken()
    builtToken
  }

  override protected def switchToScalaModeAndFetchToken() {
    modeStack.push(new ScalaMode)
    fetchScalaToken()
  }

  override protected def switchToXmlModeAndFetchToken() {
    modeStack.push(new XmlMode)
    fetchXmlToken()
  }

}

object ScalaLexer {

  def createRawLexer(s: String, forgiveErrors: Boolean = false): ScalaLexer =
    new ScalaLexer(new UnicodeEscapeReader(s, forgiveErrors), forgiveErrors)

  def tokeniseFull(file: File): (HiddenTokenInfo, List[Token]) = {
    val s = Source.fromFile(file).mkString
    tokeniseFull(s)
  }

  def tokeniseFull(s: String, forgiveErrors: Boolean = false): (HiddenTokenInfo, List[Token]) = {
    val lexer = new NewlineInferencer(new WhitespaceAndCommentsGrouper(createRawLexer(s, forgiveErrors)))
    val tokenBuffer = new ListBuffer[Token]
    var continue = true
    while (continue) {
      val token = lexer.nextToken()
      tokenBuffer += token
      if (token.tokenType == Tokens.EOF)
        continue = false
    }

    (lexer, tokenBuffer.toList)
  }

  def tokenise(s: String): List[Token] = tokeniseFull(s)._2

  def rawTokenise(s: String, forgiveErrors: Boolean = false): List[Token] = {
    val lexer = createRawLexer(s, forgiveErrors)
    var actualTokens: List[Token] = Nil
    var continue = true
    while (continue) {
      val token = lexer.nextToken()
      actualTokens ::= token
      if (token.tokenType == Tokens.EOF)
        continue = false
    }
    (actualTokens.tail).reverse
  }

  /**
   * For performance tests only
   */
  def rawTokenise2(s: String): List[Token] = {
    val lexer = new WhitespaceAndCommentsGrouper(new ScalaLexer(new UnicodeEscapeReader(s)))
    var actualTokens: List[Token] = Nil
    var continue = true
    while (lexer.hasNext) {
      val (_, token) = lexer.next()
      actualTokens ::= token
      if (token.tokenType == Tokens.EOF)
        continue = false
    }
    (actualTokens.tail).reverse
  }

}
