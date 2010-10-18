package scalariform.parser

import scalariform.lexer._
import scalariform.parser._

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

// format: +preserveSpaceBeforeArguments
class ScalaParserTest extends FlatSpec with ShouldMatchers {

  "Parser" should "not throw exception" in {
    //    parseExpression("class X")
    //    parseExpression("class X { }")
    //    parseExpression("class X { def method(n: Int) = 42 }")
    parseExpression {
      """
class C(@annotation(foo = {1 + 2}) n: Int)
"""
    }

  }

  private def parseExpression(s: String) = {
    val (lexer, tokens) = ScalaLexer.tokeniseFull(s)
    val scalaParser = new ScalaParser(tokens.toArray)
    scalaParser.compilationUnit()
  }

}
