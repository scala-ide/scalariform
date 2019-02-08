package scalariform.formatter
import scalariform.parser._

// format: OFF
class TrailingCommasTest extends AbstractFormatterTest {

  """val foos = List(
    |  f1,
    |  f2,)""" ==>
    """val foos = List(
      |  f1,
      |  f2, )"""

  //  format: ON

  override val debug = true

  def parse(parser: ScalaParser) = parser.nonLocalDefOrDcl()

  type Result = FullDefOrDcl

  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState(indentLevel = 0))

}
