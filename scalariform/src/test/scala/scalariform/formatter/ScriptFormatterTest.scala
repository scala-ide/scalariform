package scalariform.formatter

import scalariform.parser._
import scalariform.formatter._

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
  
  def getParser(parser: ScalaCombinatorParser): ScalaCombinatorParser#Parser[Result] = parser.compilationUnitOrScript
  
  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState())

}
