package scalariform.formatter

import scalariform.parser._
import scalariform.formatter._

class PackageFormatterTest extends AbstractFormatterTest {

  override val debug = false

  type Result = CompilationUnit
  
  def getParser(parser: ScalaCombinatorParser): ScalaCombinatorParser#Parser[Result] = parser.phrase(parser.compilationUnit)
  
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
}
