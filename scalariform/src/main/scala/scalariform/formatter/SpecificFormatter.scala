package scalariform.formatter

import scalariform.lexer.Tokens._
import scalariform.lexer._
import scalariform.parser._
import scalariform.utils._
import scalariform.formatter.preferences._

trait SpecificFormatter {

  def debug = false

  type Result <: AstNode

  def parse(parser: ScalaParser): Result

  def format(formatter: ScalaFormatter, result: Result): FormatResult

  def getTokens(s: String): List[Token] = ScalaLexer.tokenise(s)

  @throws(classOf[ScalaParserException])
  def format(source: String, lineDelimiter: Option[String] = None)(baseFormattingPreferences: IFormattingPreferences): String = {
    val (edits, _) = fullFormat(source, lineDelimiter)(baseFormattingPreferences)
    TextEditProcessor.runEdits(source, edits)
  }

  @throws(classOf[ScalaParserException])
  def fullFormat(source: String, lineDelimiter: Option[String] = None)(baseFormattingPreferences: IFormattingPreferences): (List[TextEdit], FormatResult) = {
    import scalariform.parser._

    val startTime = System.currentTimeMillis
    val tokens = ScalaLexer.tokenise(source)
    if (debug) {
      println
      println(source)
      println("Tokens:")
      tokens foreach println
    }

    val parser = new ScalaParser(tokens.toArray)

    val parseResult = parse(parser)

    var actualFormattingPreferences = baseFormattingPreferences
    for {
      token <- tokens
      hiddenToken ← token.associatedWhitespaceAndComments
      ToggleOption(onOrOff, optionName) ← FormatterDirectiveParser.getDirectives(hiddenToken.text)
      rawPreference ← AllPreferences.preferencesByKey.get(optionName)
      if rawPreference.preferenceType == BooleanPreference
      preference = BooleanPreference.cast(rawPreference)
    } actualFormattingPreferences = actualFormattingPreferences.setPreference(preference, onOrOff)

    require(parseResult.tokens == tokens.init, "Parse tokens differ from expected. Actual = " + parseResult.tokens + ", expected = " + tokens.init + ", parseResult = " + parseResult) // dropped EOF
    if (debug) { println("Parse result: " + parseResult) }
    val elapsedTime = System.currentTimeMillis - startTime
    //     if (debug) 
    //       println("Parse time = " + elapsedTime + "ms")
    val newlineSequence_ = lineDelimiter.getOrElse(if (source contains "\r\n") "\r\n" else "\n")

    val formatter = new ScalaFormatter() {

      def isInferredNewline(token: Token): Boolean = token.isNewline

      /** requires isInferredNewline(token) == true */
      def inferredNewlines(token: Token): HiddenTokens = token.associatedWhitespaceAndComments

      def hiddenPredecessors(token: Token): HiddenTokens = token.associatedWhitespaceAndComments

      val formattingPreferences: IFormattingPreferences = actualFormattingPreferences

      val newlineSequence = newlineSequence_

    }

    val formatResult = format(formatter, parseResult)
    if (debug) println("Format result: " + formatResult)
    val edits = formatter.writeTokens(source, tokens, formatResult)
    (edits, formatResult)
  }

}
