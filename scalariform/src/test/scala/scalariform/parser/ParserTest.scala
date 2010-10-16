package scalariform.parser

import scalariform.lexer._
import scalariform.parser._

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

// format: +preserveSpaceBeforeArguments
class ParserTest extends FlatSpec with ShouldMatchers {

  "Parser" should "throw a parse exception" in {
    evaluating { parseExpression("for {x <- b if }") } should produce[ScalaParserException]
  }

  private def parseExpression(s: String) = {
    val (lexer, tokens) = ScalaLexer.tokeniseFull(s)
    new ScalaParser(tokens.toArray).expr
  }

}
