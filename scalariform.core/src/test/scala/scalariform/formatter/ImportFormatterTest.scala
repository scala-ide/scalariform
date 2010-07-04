package scalariform.formatter

import scalariform.parser._
import scalariform.formatter._

// format: OFF
class ImportFormatterTest extends AbstractFormatterTest {

  "import foo . _" ==> "import foo._" 
  "import foo . bar" ==> "import foo.bar" 
  "import foo.{bar=>baz}" ==> "import foo.{ bar => baz }"
  "import foo.{bar=>baz},baz.biz" ==> "import foo.{ bar => baz }, baz.biz"

  """import foo.{bar => baz,
    |wibble => wobble}""" ==>
  """import foo.{
    |  bar => baz,
    |  wibble => wobble
  |}"""

  override val debug = false

  type Result = CompilationUnit
  
  def getParser(parser: ScalaCombinatorParser): ScalaCombinatorParser#Parser[Result] = parser.phrase(parser.compilationUnit)
  
  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState())

}
