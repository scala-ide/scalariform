package scalariform.formatter

import scalariform.parser.{ FullDefOrDcl, ScalaParser }
import scalariform.formatter.preferences._

// format: OFF
class FirstParameterOnNewlineFormatterTest extends AbstractFormatterTest {
  {
  implicit val formattingPreferences = FormattingPreferences.setPreference(FirstParameterOnNewline, Prevent)

  """class A(a: Int,
    |  b: String
    |)""" ==>
  """class A(a: Int,
    |  b: String)"""

  """class A(
    |  c: Int,
    |  d: String
    |)""" ==>
  """class A(c: Int,
    |  d: String)"""

  """def foo(a: Int,
    |    b: String): Unit = {}""" ==>
  """def foo(a: Int,
    |  b: String): Unit = {}"""

  """def foo(
    |  a: Int,
    |  b: String
    |): Unit = {}""" ==>
  """def foo(a: Int,
    |  b: String): Unit = {}"""
  }

  {
  implicit val formattingPreferences = FormattingPreferences.setPreference(FirstParameterOnNewline, Preserve)

  """class A(a: Int,
    |  b: String
    |)""" ==>
  """class A(a: Int,
    |  b: String)"""

  """class A(
    |  a: Int,
    |  b: String
    |)""" ==>
  """class A(
    |  a: Int,
    |  b: String)"""

  """def foo(a: Int,
    |    b: String): Unit = {}""" ==>
  """def foo(a: Int,
    |  b: String): Unit = {}"""

  """def foo(
    |  a: Int,
    |  b: String
    |): Unit = {}""" ==>
  """def foo(
    |  a: Int,
    |  b: String): Unit = {}"""
  }

  {
  implicit val formattingPreferences = FormattingPreferences.setPreference(FirstParameterOnNewline, Force)

  """class A(a: Int,
    |  b: String
    |)""" ==>
  """class A(
    |  a: Int,
    |  b: String)"""

  """class A(
    |  a: Int,
    |  b: String
    |)""" ==>
  """class A(
    |  a: Int,
    |  b: String)"""

  """def foo(a: Int,
    |    b: String): Unit = {}""" ==>
  """def foo(
    |  a: Int,
    |  b: String): Unit = {}"""

  """def foo(
    |a: Int,
    |    b: String): Unit = {}""" ==>
  """def foo(
    |  a: Int,
    |  b: String): Unit = {}"""
  }

  {
  implicit val formattingPreferences =
    FormattingPreferences.setPreference(FirstParameterOnNewline, Preserve).setPreference(AlignParameters, true)

  // Parameters left on the first line should have other parameters aligned to them.
  """def foo(a: Int,
    |    b: String,
    |c: String): Unit = {}""" ==>
  """def foo(a: Int,
    |        b: String,
    |        c: String): Unit = {}"""

  // Parameters on the next line should also be aligned properly.
  """def foo(
    |a: Int,
    |    b: String,
    | c: String): Unit = {}""" ==>
  """def foo(
    |  a: Int,
    |  b: String,
    |  c: String): Unit = {}"""
  }

  {
  implicit val formattingPreferences =
    FormattingPreferences.setPreference(FirstParameterOnNewline, Prevent).setPreference(AlignParameters, true)

  // Parameters on the next line should be pulled to the first line, and aligned.
  """def foo(
    |a: Int,
    |    b: String,
    | c: String): Unit = {}""" ==>
  """def foo(a: Int,
    |        b: String,
    |        c: String): Unit = {}"""
  }

  //  format: ON

  override val debug = false

  override def parse(parser: ScalaParser) = parser.nonLocalDefOrDcl()

  type Result = FullDefOrDcl

  override def format(formatter: ScalaFormatter, result: Result) =
    formatter.format(result)(FormatterState(indentLevel = 0))
}
