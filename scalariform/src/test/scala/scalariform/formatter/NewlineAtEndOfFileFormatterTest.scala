package scalariform.formatter

import scalariform.parser.{CompilationUnit, ScalaParser}
import scalariform.formatter.preferences.{FormattingPreferences, NewlineAtEndOfFile}

/** Tests that top-level parses add a newline when NewlineAtEndOfFile is set. */
class NewlineAtEndOfFileFormatterTest extends AbstractFormatterTest {
  type Result = CompilationUnit

  // Must parse as a full script to verify the newline formatting.
  def parse(parser: ScalaParser) = parser.scriptBody()

  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState())

  override val debug = false

  {
  implicit val formattingPreferences = FormattingPreferences.setPreference(NewlineAtEndOfFile, true)

  // No newline; should have one added.
  """import foo.bar
  |
  |class Foo {
  |}""" ==>
  """import foo.bar
  |
  |class Foo {
  |}
  |"""

  // Has newline; should stay the same.
  """import foo.bar
  |class Foo {
  |}
  |""" ==>
  """import foo.bar
  |class Foo {
  |}
  |"""
  }
}
