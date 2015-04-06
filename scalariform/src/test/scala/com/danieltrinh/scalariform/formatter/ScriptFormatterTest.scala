package com.danieltrinh.scalariform.formatter

import com.danieltrinh.scalariform.parser._
import com.danieltrinh.scalariform.formatter._

// format: OFF
class ScriptFormatterTest extends AbstractFormatterTest {

  """println("Hello world")""" ==> """println("Hello world")"""

  """def sayHi() { println("Hello")
    |}""" ==>
  """def sayHi() {
    |  println("Hello")
    |}"""

  override val debug = false

  type Result = CompilationUnit

  def parse(parser: ScalaParser) = parser.scriptBody()
  
  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState())

}
