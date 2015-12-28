package scalariform.formatter

import scalariform.parser._

abstract class AbstractExpressionFormatterTest extends AbstractFormatterTest {

  type Result = Expr

  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState())

  def parse(parser: ScalaParser) = parser.expr()

}
