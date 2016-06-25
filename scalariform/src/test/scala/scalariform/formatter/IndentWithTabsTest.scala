package scalariform.formatter

import scalariform.parser._
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
    |\tdef meth() {
    |
    |\t\tprintln("42") // wibble
    |\t\tprintln("wobble")
    |
    |\t}
    |
    |}""".replace("\\t", "\t")

  """val n = 42 +
    |3""" ==>
  """val n = 42 +
    |\t3""".replace("\\t", "\t")

  """val xml = <foo>
    |bar
    |</foo>""" ==>
  """val xml = <foo>
    |\tbar
    |</foo>""".replace("\\t", "\t")

  """foo(
    |alpha = "foo",
    |beta = "bar",
    |gamma = false)""" ==>
  """foo(
    |\talpha = "foo",
    |\tbeta = "bar",
    |\tgamma = false)""".replace("\\t", "\t")

  """foo(
    |"foo",
    |"bar",
    |false)""" ==>
  """foo(
    |\t"foo",
    |\t"bar",
    |\tfalse)""".replace("\\t", "\t")

  {
    implicit val formattingPreferences = FormattingPreferences
      .setPreference(IndentWithTabs, true)
      .setPreference(DanglingCloseParenthesis, Force)

    """foo(
      |alpha = "foo",
      |beta = "bar",
      |gamma = false)""" ==>
    """foo(
      |\talpha = "foo",
      |\tbeta = "bar",
      |\tgamma = false
      |)""".replace("\\t", "\t")

    """foo(
      |"foo",
      |"bar",
      |false)""" ==>
    """foo(
      |\t"foo",
      |\t"bar",
      |\tfalse
      |)""".replace("\\t", "\t")

    """foo(
      |\t"foo",
      |\t"bar",
      |\tfalse)""".replace("\\t", "\t") ==>
    """foo(
      |\t"foo",
      |\t"bar",
      |\tfalse
      |)""".replace("\\t", "\t")
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
      |\talpha = "foo",
      |\tbeta = "bar",
      |\tgamma = false)""".replace("\\t", "\t")

    """foo(
      |"foo",
      |"bar",
      |false
      |)""" ==>
    """foo(
      |\t"foo",
      |\t"bar",
      |\tfalse
      |)""".replace("\\t", "\t")

    """foo(
      |\t"foo",
      |\t"bar",
      |\tfalse)""".replace("\\t", "\t") ==>
    """foo(
      |\t"foo",
      |\t"bar",
      |\tfalse)""".replace("\\t", "\t")

    """sequence(
      |authors.grouped(100).map(batch =>
      |Object.get[List[SomeType]](
      |Foo / "bar" / "baz" / "qux"
      |)
      |).toList
      |).map(_.flatten)""" ==>
    """sequence(
      |\tauthors.grouped(100).map(batch =>
      |\t\tObject.get[List[SomeType]](
      |\t\t\tFoo / "bar" / "baz" / "qux"
      |\t\t)
      |\t).toList
      |).map(_.flatten)""".replace("\\t", "\t")
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
      |\talpha = "foo",
      |\tbeta = "bar",
      |\tgamma = false)""".replace("\\t", "\t")

    """foo(
      |"foo",
      |"bar",
      |false
      |)""" ==>
    """foo(
      |\t"foo",
      |\t"bar",
      |\tfalse)""".replace("\\t", "\t")

    """foo(
      |\t"foo",
      |\t"bar",
      |\tfalse
      |)""".replace("\\t", "\t") ==>
    """foo(
      |\t"foo",
      |\t"bar",
      |\tfalse)""".replace("\\t", "\t")
  }

  override val debug = false

  type Result = CompilationUnit

  def parse(parser: ScalaParser) = parser.compilationUnitOrScript

  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState())

}
