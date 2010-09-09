package scalariform.parser

import scalariform.lexer._
import scalariform.parser._

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

// format: +preserveSpaceBeforeArguments
class ParserTest extends FlatSpec with ShouldMatchers {

  "Parser" should "not throw exception" in {
    parseExpression("for {x <- b if }") should not be ('successful)
  }

  private def parseExpression(s: String) = {
    val (lexer, tokens) = ScalaLexer.tokeniseFull(s)
    val parser = new ScalaCombinatorParser
    (parser.phrase(parser.expr))(new ScalaLexerReader(tokens))
  }

}
