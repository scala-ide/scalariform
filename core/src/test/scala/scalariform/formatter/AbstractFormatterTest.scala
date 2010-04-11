package scalariform.formatter

import scalariform.parser._
import scalariform.lexer._
import scalariform.formatter.preferences._

import scala.util.parsing.input._
import scala.util.parsing.combinator._

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.TestFailedException
import org.scalatest.TestPendingException

abstract class AbstractFormatterTest extends FlatSpec with ShouldMatchers with SpecificFormatter {

  def prettyPrint(s: String): String =
    //s.replaceAll("\n", "↵\n").replaceAll("\t", "↦" ).replaceAll(" ", "▵")
    s.replaceAll("\n", "¶\n").replaceAll("\t", "↦") //.replaceAll(" ", "▲")

  implicit def string2FormatTest(s: String)(implicit formattingPreferences: IFormattingPreferences = FormattingPreferences()): FormatTest =
    FormatTest(s.stripMargin, formattingPreferences)

  override def newTestFailedException(message: String) = new TestFailedException(message = Some(message), cause = None, failedCodeStackDepth = 2)

  case class FormatTest(source: String, formattingPreferences: IFormattingPreferences) {
    require(formattingPreferences != null)
    def ==>(expectedRaw: String) {
      it should ("format >>>" + prettyPrint(source) + "<<< as >>>" + prettyPrint(expectedRaw) + "<<<") in {
        val expected = expectedRaw.stripMargin
        val actual = format(source)(formattingPreferences)
        if (debug) println("Actual = " + actual)
        if (expected != actual) {
          throw newTestFailedException("Format failure:\n ---- Expected ---- \n" + prettyPrint(expected) + "<<<\n ---- but was----- \n" + prettyPrint(actual) + "<<<\n ---- Original: ----- \n" + prettyPrint(source) + "<<<.")
        }
        val beforeTokens = ScalaLexer.tokenise(source)
        val afterTokens = ScalaLexer.tokenise(actual)
        val newlineTokenTypes = Set(Tokens.NEWLINE, Tokens.NEWLINES)
        if (beforeTokens.map(_.getType).find(!newlineTokenTypes.contains(_)) != afterTokens.map(_.getType).find(!newlineTokenTypes.contains(_)))
          throw newTestFailedException("Text as expected, but actual and expected tokens differ:\n ---- Before ---- \n" + beforeTokens + "\n ---- After ---- \n" + afterTokens + "\n")

        val actual2 = format(actual)(formattingPreferences)
        if (actual2 != actual) {
          throw newTestFailedException("Idempotency failure:\n ---- Expected ---- \n" + prettyPrint(actual) + "<<<\n ---- but was----- \n" + prettyPrint(actual2) + "<<<.")
        }
        val afterTokens2 = ScalaLexer.tokenise(actual2)
        if (afterTokens2.map(_.getType) != afterTokens.map(_.getType)) {
          throw newTestFailedException("Idempotency token inconsistency:\n ---- One ---- \n" + afterTokens2 + "\n ---- Twice ---- \n" + afterTokens2 + "\n")
        }
      }
    }
    def =/=>(expected: String): Because = {
      //println("Warning -- skipped test:\n" + source)
      new Because(expected)
    }
    class Because(expected: String) {
      def because(reason: String) = {
        //println("because " + reason)
        it should ("format >>>" + prettyPrint(source) + "<<< as >>>" + prettyPrint(expected) + "<<<, but did not because " + reason) in {
          throw new TestPendingException
        }
      }
    }
  }

}
