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
  |class SettingOn {
  |}""" ==>
  """import foo.bar
  |
  |class SettingOn {
  |}
  |"""

  // Has newline; should stay the same.
  """import foo.bar
  |class SettingOn {
  |}
  |""" ==>
  """import foo.bar
  |class SettingOn {
  |}
  |"""
  }

  {
  implicit val formattingPreferences = FormattingPreferences.setPreference(NewlineAtEndOfFile, false)

  // No newline; should stay the same.
  """import foo.bar
  |
  |class SettingOff {
  |}""" ==>
  """import foo.bar
  |
  |class SettingOff {
  |}"""

  // Has newline; should stay the same (preference off doesn't strip newlines that exist).
  """import foo.bar
  |class SettingOff {
  |}
  |""" ==>
  """import foo.bar
  |class SettingOff {
  |}
  |"""
  }
}
