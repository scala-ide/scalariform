package scalariform.lexer

import scalariform._
import scalariform.lexer.Tokens._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class ScalaLexerTest extends FlatSpec with ShouldMatchers {

  implicit def string2TestString(s: String)(implicit forgiveErrors: Boolean = false, scalaVersion: ScalaVersion = ScalaVersions.DEFAULT) =
    new TestString(s, forgiveErrors, scalaVersion)

  "" producesTokens ()

  "println" producesTokens (VARID)

  "lazy" producesTokens (LAZY)

  "println;println" producesTokens (VARID, SEMI, VARID)

  "foo_+" producesTokens (VARID)

  "++" producesTokens (VARID)

  "*=" producesTokens (VARID)

  "/" producesTokens (VARID)

  "foo/bar" producesTokens (VARID, VARID, VARID)

  "*/" producesTokens (VARID)

  "*/+" producesTokens (VARID)

  "foo  bar   baz" producesTokens (VARID, WS, VARID, WS, VARID)

  "  " producesTokens (WS)

  "// comment" producesTokens (LINE_COMMENT)

  "//" producesTokens (LINE_COMMENT)

  "foo// comment" producesTokens (VARID, LINE_COMMENT)

  "foo // comment" producesTokens (VARID, WS, LINE_COMMENT)

  """foo// comment
    |bar//comment""" producesTokens (VARID, LINE_COMMENT, VARID, LINE_COMMENT)

  "foo/* comment */bar" producesTokens (VARID, MULTILINE_COMMENT, VARID)

  "/* bar /* baz */ var */" producesTokens (MULTILINE_COMMENT)

  "/**/" producesTokens (MULTILINE_COMMENT)

  "`yield`" producesTokens (VARID)

  """"foobar"""" producesTokens (STRING_LITERAL)

  "\"\"\"f\"o\"o\"\"\"" producesTokens (STRING_LITERAL)

  """"\""""" producesTokens (STRING_LITERAL)

  "\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"" producesTokens (STRING_LITERAL)

  "foo.bar.baz" producesTokens (VARID, DOT, VARID, DOT, VARID)

  ".1234" producesTokens (FLOATING_POINT_LITERAL)
  ".1234e2" producesTokens (FLOATING_POINT_LITERAL)
  ".1234e+2" producesTokens (FLOATING_POINT_LITERAL)
  ".1e-2" producesTokens (FLOATING_POINT_LITERAL)
  ".1e+2345f" producesTokens (FLOATING_POINT_LITERAL)
  ".1e+2345d" producesTokens (FLOATING_POINT_LITERAL)

  "100" producesTokens (INTEGER_LITERAL)
  "1" producesTokens (INTEGER_LITERAL)
  "1L" producesTokens (INTEGER_LITERAL)
  "0" producesTokens (INTEGER_LITERAL)
  "0L" producesTokens (INTEGER_LITERAL)
  "0x2345" producesTokens (INTEGER_LITERAL)
  "0x" producesTokens (INTEGER_LITERAL)
  "0x32413L" producesTokens (INTEGER_LITERAL)
  "1Lfoo" producesTokens (INTEGER_LITERAL, VARID)
  "0x1Lfoo" producesTokens (INTEGER_LITERAL, VARID)

  "0.1234" producesTokens (FLOATING_POINT_LITERAL)
  "0.1234e2" producesTokens (FLOATING_POINT_LITERAL)
  "0.1234e+2" producesTokens (FLOATING_POINT_LITERAL)
  "0.1e-2" producesTokens (FLOATING_POINT_LITERAL)
  "0.1e+2345f" producesTokens (FLOATING_POINT_LITERAL)
  "0.1e+2345d" producesTokens (FLOATING_POINT_LITERAL)

  "10e2" producesTokens (FLOATING_POINT_LITERAL)
  "10e+2" producesTokens (FLOATING_POINT_LITERAL)
  "10e-2" producesTokens (FLOATING_POINT_LITERAL)
  "10e+2345f" producesTokens (FLOATING_POINT_LITERAL)
  "10e+2345d" producesTokens (FLOATING_POINT_LITERAL)

  "22.`yield`" producesTokens (INTEGER_LITERAL, DOT, VARID)
  "42.toString" producesTokens (INTEGER_LITERAL, DOT, VARID)

  {
    implicit val scalaVersion = ScalaVersions.Scala_2_9
    "5.f" producesTokens (FLOATING_POINT_LITERAL)
    "5.d" producesTokens (FLOATING_POINT_LITERAL)
    "5." producesTokens (FLOATING_POINT_LITERAL)
  }

  {
    implicit val scalaVersion = ScalaVersions.Scala_2_11
    "5.f" producesTokens (INTEGER_LITERAL, DOT, VARID)
    "5.d" producesTokens (INTEGER_LITERAL, DOT, VARID)
    "5." producesTokens (INTEGER_LITERAL, DOT)
  }

  {
    implicit val scalaVersion = ScalaVersions.Scala_2_9
    """ X s"" """ producesTokens (WS, VARID, WS, VARID, STRING_LITERAL, WS)
  }

  {
    implicit val scalaVersion = ScalaVersions.Scala_2_10
    """ X s"" """ producesTokens (WS, VARID, WS, INTERPOLATION_ID, STRING_LITERAL, WS)
    """ X s "$foo" """ producesTokens (WS, VARID, WS, VARID, WS, STRING_LITERAL, WS)
    """ s"$foo" """ producesTokens (WS, INTERPOLATION_ID, STRING_PART, VARID, STRING_LITERAL, WS)
    """ s"$$" """ producesTokens (WS, INTERPOLATION_ID, STRING_LITERAL, WS)
    """ s"?{foo}" """.replace('?', '$') producesTokens (WS, INTERPOLATION_ID, STRING_PART, LBRACE, VARID, RBRACE, STRING_LITERAL, WS)
    """ s"?{s"?{x}"}" """.replace('?', '$') producesTokens (WS, INTERPOLATION_ID, STRING_PART, LBRACE, INTERPOLATION_ID, STRING_PART, LBRACE, VARID, RBRACE, STRING_LITERAL, RBRACE, STRING_LITERAL, WS)

    """ s"$this" """ producesTokens (WS, INTERPOLATION_ID, STRING_PART, THIS, STRING_LITERAL, WS)

    """ s"$_" """ producesTokens (WS, INTERPOLATION_ID, STRING_PART, USCORE, STRING_LITERAL, WS)

    <t>s""""""</t>.text producesTokens (INTERPOLATION_ID, STRING_LITERAL)
    <t>s"""""""""</t>.text producesTokens (INTERPOLATION_ID, STRING_LITERAL)
    <t>s""" $foo """</t>.text producesTokens (INTERPOLATION_ID, STRING_PART, VARID, STRING_LITERAL)

  }

  "'f'" producesTokens (CHARACTER_LITERAL)
  """'\n'""" producesTokens (CHARACTER_LITERAL)
  """'\025'""" producesTokens (CHARACTER_LITERAL)

  "'symbol" producesTokens (SYMBOL_LITERAL)
  "'yield" producesTokens (SYMBOL_LITERAL)

  "private val tokenTextBuffer = new StringBuilder" producesTokens (PRIVATE, WS, VAL, WS, VARID, WS, EQUALS, WS, NEW, WS, VARID)

  """println("bob")
println("foo")""" producesTokens (VARID, LPAREN, STRING_LITERAL, RPAREN, WS, VARID, LPAREN, STRING_LITERAL, RPAREN)

  "\\u0061" producesTokens (VARID)
  "\\uuuuuuuuuuuuuuuuuuuuuuuuu0061" producesTokens (VARID)
  "\"\\u0061\"" producesTokens (STRING_LITERAL)
  "\"\\u000a\"" producesTokens (STRING_LITERAL)

  "<:" producesTokens (SUBTYPE)
  "<foo />" producesTokens (XML_START_OPEN, XML_NAME, XML_WHITESPACE, XML_EMPTY_CLOSE)
  "<foo></foo>" producesTokens (XML_START_OPEN, XML_NAME, XML_TAG_CLOSE, XML_END_OPEN, XML_NAME, XML_TAG_CLOSE)
  "<foo></foo  >" producesTokens (XML_START_OPEN, XML_NAME, XML_TAG_CLOSE, XML_END_OPEN, XML_NAME, XML_WHITESPACE, XML_TAG_CLOSE)
  "<foo attr='val'/>" producesTokens (XML_START_OPEN, XML_NAME, XML_WHITESPACE, XML_NAME, XML_ATTR_EQ, XML_ATTR_VALUE, XML_EMPTY_CLOSE)
  "<foo>bar</foo>" producesTokens (XML_START_OPEN, XML_NAME, XML_TAG_CLOSE, XML_PCDATA, XML_END_OPEN, XML_NAME, XML_TAG_CLOSE)
  "<foo><bar>baz</bar></foo>" producesTokens (XML_START_OPEN, XML_NAME, XML_TAG_CLOSE, XML_START_OPEN, XML_NAME, XML_TAG_CLOSE, XML_PCDATA,
    XML_END_OPEN, XML_NAME, XML_TAG_CLOSE, XML_END_OPEN, XML_NAME, XML_TAG_CLOSE)

  "10 + <foo/>" producesTokens (INTEGER_LITERAL, WS, PLUS, WS, XML_START_OPEN, XML_NAME, XML_EMPTY_CLOSE)

  "<foo>bar{ baz }boz</foo>" producesTokens (XML_START_OPEN, XML_NAME, XML_TAG_CLOSE, XML_PCDATA, LBRACE, WS, VARID, WS, RBRACE, XML_PCDATA,
    XML_END_OPEN, XML_NAME, XML_TAG_CLOSE)
  "<foo attr={n}/>" producesTokens (XML_START_OPEN, XML_NAME, XML_WHITESPACE, XML_NAME, XML_ATTR_EQ, LBRACE, VARID, RBRACE, XML_EMPTY_CLOSE)

  "10 + <foo/> + 20" producesTokens (INTEGER_LITERAL, WS, PLUS, WS, XML_START_OPEN, XML_NAME, XML_EMPTY_CLOSE, WS, PLUS, WS, INTEGER_LITERAL)

  "10 + <foo>bar</foo> + 20" producesTokens (INTEGER_LITERAL, WS, PLUS, WS,
    XML_START_OPEN, XML_NAME, XML_TAG_CLOSE, XML_PCDATA, XML_END_OPEN, XML_NAME, XML_TAG_CLOSE,
    WS, PLUS, WS, INTEGER_LITERAL)

  "10 + <foo>{1 + <foo/> + 2}</foo> + 20" producesTokens (INTEGER_LITERAL, WS, PLUS, WS,
    XML_START_OPEN, XML_NAME, XML_TAG_CLOSE,
    LBRACE, INTEGER_LITERAL, WS, PLUS, WS,
    XML_START_OPEN, XML_NAME, XML_EMPTY_CLOSE,
    WS, PLUS, WS, INTEGER_LITERAL, RBRACE,
    XML_END_OPEN, XML_NAME, XML_TAG_CLOSE,
    WS, PLUS, WS, INTEGER_LITERAL)

  "<a><b/></a>" producesTokens (XML_START_OPEN, XML_NAME, XML_TAG_CLOSE,
    XML_START_OPEN, XML_NAME, XML_EMPTY_CLOSE,
    XML_END_OPEN, XML_NAME, XML_TAG_CLOSE)

  "<a>/</a>" producesTokens (XML_START_OPEN, XML_NAME, XML_TAG_CLOSE, XML_PCDATA, XML_END_OPEN, XML_NAME, XML_TAG_CLOSE)
  "<a>{}/</a>" producesTokens (XML_START_OPEN, XML_NAME, XML_TAG_CLOSE, LBRACE, RBRACE, XML_PCDATA, XML_END_OPEN, XML_NAME, XML_TAG_CLOSE)

  "<a><!-- comment --></a>" producesTokens (XML_START_OPEN, XML_NAME, XML_TAG_CLOSE, XML_COMMENT, XML_END_OPEN, XML_NAME, XML_TAG_CLOSE)

  "<![CDATA[<greeting>Hello, world!</greeting>]]>" producesTokens (XML_CDATA)

  "3 + <!-- foo --> + 4" producesTokens (INTEGER_LITERAL, WS, PLUS, WS, XML_COMMENT, WS, PLUS, WS, INTEGER_LITERAL)

  "<a>{{ = }}</a>" producesTokens (XML_START_OPEN, XML_NAME, XML_TAG_CLOSE, XML_PCDATA, XML_END_OPEN, XML_NAME, XML_TAG_CLOSE)

  "0X1234" producesTokens (INTEGER_LITERAL)

  """<xml:unparsed>&<<>""^%@$!#</xml:unparsed>""" producesTokens (XML_UNPARSED)
  """3 + <xml:unparsed>&<<>""^%@$!#</xml:unparsed> + 3""" producesTokens (INTEGER_LITERAL, WS, PLUS, WS, XML_UNPARSED, WS, PLUS, WS,
    INTEGER_LITERAL)

  """<?this is a pi foo bar = && {{ ?>""" producesTokens (XML_PROCESSING_INSTRUCTION)

  """<foo/>

     <bar/>""" producesTokens (XML_START_OPEN, XML_NAME, XML_EMPTY_CLOSE, XML_PCDATA, XML_START_OPEN, XML_NAME, XML_EMPTY_CLOSE)

  """<foo/>
     <bar/>""" producesTokens (XML_START_OPEN, XML_NAME, XML_EMPTY_CLOSE, XML_PCDATA, XML_START_OPEN, XML_NAME, XML_EMPTY_CLOSE)

  """<!-- -->
     <bar/>""" producesTokens (XML_COMMENT, XML_PCDATA, XML_START_OPEN, XML_NAME, XML_EMPTY_CLOSE)

  "for(<book/><-Nil)Nil" producesTokens (FOR, LPAREN, XML_START_OPEN, XML_NAME, XML_EMPTY_CLOSE, LARROW, VARID, RPAREN, VARID)

  "a -> b" producesTokens(VARID, WS, VARID, WS, VARID)

  "import Foo.->" producesTokens(IMPORT, WS, VARID, DOT, VARID)

  "val x = ->" producesTokens(VAL, WS, VARID, WS, EQUALS, WS, VARID)

  "val x: Any = ->" producesTokens(VAL, WS, VARID, COLON, WS, VARID, WS, EQUALS, WS, VARID)

  "val -> : Any = x" producesTokens(VAL, WS, VARID, WS, COLON, WS, VARID, WS, EQUALS, WS, VARID)

  "a → b" producesTokens(VARID, WS, VARID, WS, VARID)

  "\"\\u001A\"" producesTokens (STRING_LITERAL)

  "\"\"\"\\u001A\"\"\"" producesTokens (STRING_LITERAL)

  "foo+\\u0061+bar" producesTokens (VARID, PLUS, VARID, PLUS, VARID)

  "-5f.max(2)" producesTokens (MINUS, FLOATING_POINT_LITERAL, DOT, VARID, LPAREN, INTEGER_LITERAL, RPAREN)
  "-5f max(2)" producesTokens (MINUS, FLOATING_POINT_LITERAL, WS, VARID, LPAREN, INTEGER_LITERAL, RPAREN)
  "-5.max(2)" producesTokens (MINUS, INTEGER_LITERAL, DOT, VARID, LPAREN, INTEGER_LITERAL, RPAREN)
  "-5 max(2)" producesTokens (MINUS, INTEGER_LITERAL, WS, VARID, LPAREN, INTEGER_LITERAL, RPAREN)

  "Lexer" should "throw a lexer exception" in {
    evaluating { ScalaLexer.rawTokenise("\"\"\"") } should produce[ScalaLexerException]
    evaluating { ScalaLexer.rawTokenise("<?") } should produce[ScalaLexerException]
    evaluating { ScalaLexer.rawTokenise("<xml:unparsed>") } should produce[ScalaLexerException]
  }

  {
    implicit val forgiveErrors = true

    "\"\"\"" producesTokens (STRING_LITERAL)
    "'" producesTokens (CHARACTER_LITERAL)
    "\"unclosed" producesTokens (STRING_LITERAL)
    "\\ufoob" producesTokens (WS)
    "`unclosed" producesTokens (VARID)
    "<?" producesTokens (XML_PROCESSING_INSTRUCTION)
    "<xml:unparsed>" producesTokens (XML_UNPARSED)

  }

  class TestString(s: String, forgiveErrors: Boolean = false, scalaVersion: ScalaVersion = ScalaVersions.DEFAULT) {

    def producesTokens(toks: TokenType*)() {
      check(s.stripMargin, toks.toList)
    }

    private def check(s: String, expectedTokens: List[TokenType]) {
      it should ("tokenise >>>" + s + "<<< as >>>" + expectedTokens + "<<< forgiveErrors = " + forgiveErrors + ", scalaVersion = " + scalaVersion) in {
        val actualTokens: List[Token] = ScalaLexer.rawTokenise(s, forgiveErrors, scalaVersion.toString)
        val actualTokenTypes = actualTokens.map(_.tokenType)
        require(actualTokenTypes.last == EOF, "Last token must be EOF, but was " + actualTokens.last.tokenType)
        require(actualTokenTypes.count(_ == EOF) == 1, "There must only be one EOF token")
        val reconstitutedSource = actualTokens.init.map(_.rawText).mkString
        require(actualTokenTypes.init == expectedTokens, "Tokens do not match. Expected " + expectedTokens + ", but was " + actualTokenTypes.init)
        require(s == reconstitutedSource, "tokens do not partition text correctly: " + s + " vs " + reconstitutedSource)
      }
    }

  }

}
