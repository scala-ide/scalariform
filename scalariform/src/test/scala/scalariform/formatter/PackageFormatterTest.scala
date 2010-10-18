package scalariform.formatter

import scalariform.parser._
import scalariform.formatter._
import scalariform.formatter.preferences._

// format: OFF
class PackageFormatterTest extends AbstractFormatterTest {

  override val debug = false

  type Result = CompilationUnit

  def parse(parser: ScalaParser) = parser.compilationUnit()
  
  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState())

  "" ==> ""

  """package foo {
    |package bar {
    |class Baz
    |}
    |}""" ==>
  """package foo {
    |  package bar {
    |    class Baz
    |  }
    |}"""

  "package a {}class B" =/=> "package a {} class B" because "maybe it is, maybe it isn't legal Scala"

  "package foo" ==> "package foo"

  """/* foo */
    |package wibble""" ==>
  """/* foo */
    |package wibble"""

  """package a 
     |{}""" ==>
  """package a {}"""

  {

  implicit val formattingPreferences = FormattingPreferences.setPreference(IndentPackageBlocks, false)

  """package foo {
    |package bar {
    |class Baz
    |}
    |}""" ==>
  """package foo {
    |package bar {
    |class Baz
    |}
    |}"""

  }


}
