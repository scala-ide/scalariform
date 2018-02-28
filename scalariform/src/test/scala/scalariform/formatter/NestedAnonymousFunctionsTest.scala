package scalariform.formatter

import scalariform.formatter.preferences._
import scalariform.parser._


class NestedAnonymousFunctionsTest extends AbstractFormatterTest {
  // format: OFF
  {
    implicit val formattingPreferences = FormattingPreferences
      .setPreference(NewlinesAtNestedAnonymousFunctions, Force)

    """def foo: Int => Int => String = { x: Int => y: Int =>
      |  "x"
      |}""" ==>
      """def foo: Int => Int => String = {
        |  x: Int =>
        |    y: Int =>
        |      "x"
        |}"""
  }

  {
    implicit val formattingPreferences = FormattingPreferences
      .setPreference(NewlinesAtNestedAnonymousFunctions, Prevent)

    """def bar: Int => Int => String = {
      |  x: Int =>
      |    y: Int =>
      |      "x"
      |}""" ==>
      """def bar: Int => Int => String = { x: Int => y: Int =>
        |  "x"
        |}"""
  }

  {
    implicit val formattingPreferences = FormattingPreferences
      .setPreference(NewlinesAtNestedAnonymousFunctions, Preserve)

    """def baz: Int => Int => String = { x: Int =>
      |  y: Int =>
      |    "x"
      |}""" ==>
      """def baz: Int => Int => String = { x: Int =>
        |  y: Int =>
        |    "x"
        |}"""

    """def quux: Int => Int => String = { x: Int => y: Int =>
      |  "x"
      |}""" ==>
      """def quux: Int => Int => String = { x: Int => y: Int =>
        |  "x"
        |}"""
  }
  //  format: ON

  override val debug = false

  def parse(parser: ScalaParser) = parser.nonLocalDefOrDcl()

  type Result = FullDefOrDcl

  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState())

}
