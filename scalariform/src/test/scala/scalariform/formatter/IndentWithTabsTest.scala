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


  override val debug = false

  type Result = CompilationUnit

  def parse(parser: ScalaParser) = parser.compilationUnitOrScript
  
  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState())

}
