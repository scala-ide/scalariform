package scalariform.formatter

import preferences.{PreserveDanglingCloseParenthesis, FormattingPreferences}
import scalariform.parser.{FullDefOrDcl, ScalaParser}

class MiscFormatterTest extends AbstractFormatterTest {

  {
    implicit val formattingPreferences = FormattingPreferences.setPreference(PreserveDanglingCloseParenthesis, true)

    """class Foo(
      |  bar: String,
      |  baz: String
      |)""" ==>
      """class Foo(
        |  bar: String,
        |  baz: String
        |)"""

    """class Foo(
      |  bar: String,
      |  baz: String)""" ==>
      """class Foo(
        |  bar: String,
        |  baz: String)"""

    """class Foo(
      |)""" ==>
      """class Foo()"""


  }

  def parse(parser: ScalaParser) = parser.nonLocalDefOrDcl()

  type Result = FullDefOrDcl

  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState(indentLevel = 0))

}
