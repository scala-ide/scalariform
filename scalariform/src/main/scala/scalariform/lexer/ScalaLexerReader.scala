package scalariform.lexer

import scala.util.parsing.input._
import scalariform.lexer.Tokens._

class ScalaLexerReader(val tokens: List[Token]) extends Reader[Token] {

  def first: Token = tokens.head

  def rest: Reader[Token] = new ScalaLexerReader(tokens.tail)

  def pos: Position = new ScalaLexerPosition(first)

  def atEnd: Boolean = tokens.isEmpty || tokens.head.tokenType == EOF

  private class ScalaLexerPosition(token: Token) extends Position {

    def line: Int = -1

    def column: Int = -1

    protected def lineContents: String = token.rawText

    override def longString: String = lineContents

  }

}

