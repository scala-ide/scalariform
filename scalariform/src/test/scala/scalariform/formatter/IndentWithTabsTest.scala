package scalariform.formatter

import scalariform.parser._
import scalariform.formatter._
import scalariform.formatter.preferences._

// format: OFF
class IndentWithTabsTest extends AbstractFormatterTest {

  implicit val formattingPreferences = FormattingPreferences.setPreference(IndentWithTabs, true)

  """class A {
    |
    |def meth() {
    |
    |println("42") // wibble
    |println("wobble")
    |
    |}
    |
    |}""" ==>
  """class A {
    |
    |	def meth() {
    |
    |		println("42") // wibble
    |		println("wobble")
    |
    |	}
    |
    |}"""

  """val n = 42 +
    |3""" ==>
  """val n = 42 +
    |	3"""

  """val xml = <foo>
    |bar
    |</foo>""" ==>
  """val xml = <foo>
    |	bar
    |</foo>"""

  """foo(
    |alpha = "foo",
    |beta = "bar",
    |gamma = false)""" ==>
  """foo(
    |	alpha = "foo",
    |	beta = "bar",
    |	gamma = false
    |)"""

  """foo(
    |"foo",
    |"bar",
    |false)""" ==>
  """foo(
    |	"foo",
    |	"bar",
    |	false
    |)"""

  {
    implicit val formattingPreferences = FormattingPreferences
      .setPreference(IndentWithTabs, true)
      .setPreference(DanglingCloseParenthesis, Force)

    """foo(
      |alpha = "foo",
      |beta = "bar",
      |gamma = false)""" ==>
    """foo(
      |	alpha = "foo",
      |	beta = "bar",
      |	gamma = false
      |)"""

    """foo(
      |"foo",
      |"bar",
      |false)""" ==>
    """foo(
      |	"foo",
      |	"bar",
      |	false
      |)"""

    """foo(
      |	"foo",
      |	"bar",
      |	false)""" ==>
    """foo(
      |	"foo",
      |	"bar",
      |	false
      |)"""
  }

  {
    implicit val formattingPreferences = FormattingPreferences
      .setPreference(IndentWithTabs, true)
      .setPreference(DanglingCloseParenthesis, Preserve)

    """foo(
      |alpha = "foo",
      |beta = "bar",
      |gamma = false)""" ==>
    """foo(
      |	alpha = "foo",
      |	beta = "bar",
      |	gamma = false)"""

    """foo(
      |"foo",
      |"bar",
      |false)""" ==>
    """foo(
      |	"foo",
      |	"bar",
      |	false)"""

    """foo(
      |	"foo",
      |	"bar",
      |	false)""" ==>
    """foo(
      |	"foo",
      |	"bar",
      |	false)"""
  }

  {
    implicit val formattingPreferences = FormattingPreferences
      .setPreference(IndentWithTabs, true)
      .setPreference(DanglingCloseParenthesis, Prevent)

    """foo(
      |alpha = "foo",
      |beta = "bar",
      |gamma = false
      |)""" ==>
    """foo(
      |	alpha = "foo",
      |	beta = "bar",
      |	gamma = false)"""

    """foo(
      |"foo",
      |"bar",
      |false
      |)""" ==>
    """foo(
      |	"foo",
      |	"bar",
      |	false)"""

    """foo(
      |	"foo",
      |	"bar",
      |	false
      |)""" ==>
    """foo(
      |	"foo",
      |	"bar",
      |	false)"""
  }


  override val debug = false

  type Result = CompilationUnit

  def parse(parser: ScalaParser) = parser.compilationUnitOrScript
  
  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState())

}
