package scalariform.parser

import scalariform.lexer._

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

  "Parser" should "produce a parse exception on a trailing close brace" in {
    evaluating { parseCompilationUnit("class A{}}") } should produce[ScalaParserException]
  }

  "Parser" should "not throw an exception" in {
    parseExpression("{ case List[String]() => 12 }")
  }

  // See issue #60
  "Parser" should "not throw an exception on case block ending with decl" in {
    parseExpression("""
                args(0) match {
                        case "blah" =>
                                val x = args(0)
                        case _ =>
                                println("not blah")
                }
    """)
  }

  "Parser" should "throw a parse exception in bad package blocks" in {
    evaluating { parseCompilationUnit("package a {} package b {}") } should produce[ScalaParserException]
  }

  // issue #44
  "Parser" should "allow qualified type parameter in pattern matching" in {
    parseExpression("""
    {
      case List[scala.Int]() => 1
      case _: List[scala.Int] => 2
    }
    """)
  }

  private def parser(s: String) = new ScalaParser(ScalaLexer.tokenise(s).toArray)
  private def parseExpression(s: String) = parser(s).expr
  private def parseCompilationUnit(s: String) = parser(s).compilationUnit

}
