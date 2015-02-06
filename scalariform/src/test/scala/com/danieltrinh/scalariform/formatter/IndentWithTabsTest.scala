package com.danieltrinh.scalariform.formatter

import com.danieltrinh.scalariform.parser._
import com.danieltrinh.scalariform.formatter._
import com.danieltrinh.scalariform.formatter.preferences._

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
