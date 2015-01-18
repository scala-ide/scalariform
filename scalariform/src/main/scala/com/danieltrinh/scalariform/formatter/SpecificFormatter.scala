package com.danieltrinh.scalariform.formatter

import com.danieltrinh.scalariform.lexer.Tokens._
import com.danieltrinh.scalariform.lexer._
import com.danieltrinh.scalariform.parser._
import com.danieltrinh.scalariform.utils._
import com.danieltrinh.scalariform.formatter.preferences._
import com.danieltrinh.scalariform.ScalaVersions

trait SpecificFormatter {

  def debug = false

  type Result <: AstNode

  def parse(parser: ScalaParser): Result

  def format(formatter: ScalaFormatter, result: Result): FormatResult

  @throws(classOf[ScalaParserException])
  def format(source: String, lineDelimiter: Option[String] = None, scalaVersion: String = ScalaVersions.DEFAULT_VERSION)(baseFormattingPreferences: IFormattingPreferences): String = {
    val (edits, _) = fullFormat(source, lineDelimiter, scalaVersion)(baseFormattingPreferences)
    TextEditProcessor.runEdits(source, edits)
  }

  @throws(classOf[ScalaParserException])
  def fullFormat(source: String, lineDelimiter: Option[String] = None, scalaVersion: String = ScalaVersions.DEFAULT_VERSION)(baseFormattingPreferences: IFormattingPreferences): (List[TextEdit], FormatResult) = {
    import com.danieltrinh.scalariform.parser._

    val startTime = System.currentTimeMillis
    val tokens = ScalaLexer.tokenise(source, scalaVersion = scalaVersion)
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
      token ← tokens
      hiddenToken ← token.associatedWhitespaceAndComments
      ToggleOption(onOrOff, optionName) ← FormatterDirectiveParser.getDirectives(hiddenToken.text)
      rawPreference ← AllPreferences.preferencesByKey.get(optionName)
      if rawPreference.preferenceType == BooleanPreference
      preference = BooleanPreference.cast(rawPreference)
    } actualFormattingPreferences = actualFormattingPreferences.setPreference(preference, onOrOff)

    val parsedTokens = parseResult.tokens.filter(_.tokenType != EOF)
    require(parsedTokens == tokens.init /* <-- drop EOF */ , "Parse tokens differ from expected.\n  Actual = \n" +
      parsedTokens.mkString("\n") + "\n  expected = \n" + tokens.init.mkString("\n") + "\n  parseResult = \n" +
      parseResult)

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
