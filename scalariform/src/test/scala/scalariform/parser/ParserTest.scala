package scalariform.parser

import scalariform.lexer._
import org.scalatest.{FlatSpec, Matchers}

// format: +preserveSpaceBeforeArguments
class ParserTest extends FlatSpec with Matchers {

  "Parser" should "throw a parse exception" in {
    an [ScalaParserException] should be thrownBy parseExpression("for {x <- b if }")
  }

  "Parser" should "throw a parse exception for empty match " in {
    an [ScalaParserException] should be thrownBy parseExpression("a match { }")
  }

  "Parser" should "produce a parse exception on a trailing close brace" in {
    an [ScalaParserException] should be thrownBy parseCompilationUnit("class A{}}")
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
    (the [ScalaParserException] thrownBy parseCompilationUnit("package a {} package b {}")).line shouldBe 1
  }

  "Parser" should "indicate the line number" in {
    (the [ScalaParserException] thrownBy parseCompilationUnit(
      """class Trivial {
           def method(): Unit = {)
         }"""
    )).line shouldBe 2

    (the [ScalaParserException] thrownBy parseExpression(
    """args(0) match {
         case "blah" =>
           val x = args(0)
         case _ } // syntax error here
           println("not blah")
       }
    """)).line shouldBe 4
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

  // issue #265
  "Parser" should "handle deep complex expressions" in {
    parseExpression("o.p(a" + (",a" * 3000) + ")")
  }

  private def parser(s: String) = new ScalaParser(ScalaLexer.tokenise(s).toArray)
  private def parseExpression(s: String) = parser(s).expr()
  private def parseCompilationUnit(s: String) = parser(s).compilationUnit()

}
