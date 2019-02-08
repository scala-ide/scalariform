package scalariform.formatter

import scalariform.parser._
import scalariform.formatter.preferences._

// format: OFF
class PackageFormatterTest extends AbstractFormatterTest {

  override val debug = false

  type Result = CompilationUnit

  def parse(parser: ScalaParser): Result = parser.compilationUnit()

  def format(formatter: ScalaFormatter, result: Result): FormatResult = formatter.format(result)(FormatterState())

  "" ==> ""

  "package foo . bar . baz" ==> "package foo.bar.baz"

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

  "package foo" ==> "package foo"

  """/* foo */
    |package wibble""" ==>
  """/* foo */
    |package wibble"""

  """package a
     |{}""" ==>
  """package a {}"""

  """package a {}
     |""" ==>
  """package a {}
     |"""

  {

  implicit val formattingPreferences: FormattingPreferences =
    FormattingPreferences.setPreference(IndentPackageBlocks, false)

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
