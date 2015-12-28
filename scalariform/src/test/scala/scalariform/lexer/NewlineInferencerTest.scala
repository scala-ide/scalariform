package scalariform.lexer

import scalariform._
import scalariform.lexer.Tokens._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

/**
 * Test full tokeniser, including newline inferencing.
 */
class NewlineInferencerTest extends FlatSpec with ShouldMatchers {

  implicit def string2TestString(s: String)(implicit forgiveErrors: Boolean = false, scalaVersion: ScalaVersion = ScalaVersions.DEFAULT) =
    new TestString(s, forgiveErrors, scalaVersion);

  // See issue #60
  """
     a match {
       case b =>
         val c = d
       case e =>
     }""" shouldProduceTokens (
    VARID, MATCH, LBRACE,
    CASE, VARID, ARROW,
    VAL, VARID, EQUALS, VARID, NEWLINE,
    CASE, VARID, ARROW,
    RBRACE)

  class TestString(s: String, forgiveErrors: Boolean = false, scalaVersion: ScalaVersion = ScalaVersions.DEFAULT) {

    def shouldProduceTokens(toks: TokenType*)() {
      check(s.stripMargin, toks.toList)
    }

    private def check(s: String, expectedTokens: List[TokenType]) {
      it should ("tokenise >>>" + s + "<<< as >>>" + expectedTokens + "<<< forgiveErrors = " + forgiveErrors + ", scalaVersion = " + scalaVersion) in {
        val actualTokens: List[Token] = ScalaLexer.tokenise(s, forgiveErrors, scalaVersion.toString)
        val actualTokenTypes = actualTokens.map(_.tokenType)
        require(actualTokenTypes.last == EOF, "Last token must be EOF, but was " + actualTokens.last.tokenType)
        require(actualTokenTypes.count(_ == EOF) == 1, "There must only be one EOF token")
        require(actualTokenTypes.init == expectedTokens, "Tokens do not match. Expected " + expectedTokens + ", but was " + actualTokenTypes.init)
      }
    }

  }

}
