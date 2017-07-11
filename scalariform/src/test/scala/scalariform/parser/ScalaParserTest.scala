package scalariform.parser

import scalariform.lexer._
import org.scalatest.{FlatSpec, Matchers}

// format: +preserveSpaceBeforeArguments
class ScalaParserTest extends FlatSpec with Matchers {

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
    val tokens = ScalaLexer.tokenise(s)
    val scalaParser = new ScalaParser(tokens.toArray)
    scalaParser.compilationUnit()
  }

}
