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

  """package a.b.c.d.e.f
    |""" ==>
  """package a.b.c.d
    |package e
    |package f
    |"""

  """package com.company.analytics.math.curves.interpolators
    |""" ==>
  """package com.company.analytics.math
    |package curves
    |package interpolators
    |"""

  """package a.b.c.d.e.f
    |package g.h
    |""" ==>
  """package a.b.c.d
    |package e
    |package f
    |package g
    |package h
    |"""

}
