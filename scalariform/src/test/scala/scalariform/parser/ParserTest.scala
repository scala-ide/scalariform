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

  "Parser" should "throw a parse exception for empty match " in {
    evaluating { parseExpression("a match { }") } should produce[ScalaParserException]
  }

  "Parser" should "not throw an exception" in {
    parseExpression("{ case List[String]() => 12 }")
  }

  private def parseExpression(s: String) = {
    val (_, tokens) = ScalaLexer.tokeniseFull(s)
    new ScalaParser(tokens.toArray).expr
  }

}
