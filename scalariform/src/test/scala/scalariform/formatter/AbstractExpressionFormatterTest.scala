package scalariform.formatter

import scalariform.parser._

abstract class AbstractExpressionFormatterTest extends AbstractFormatterTest {

  type Result = Expr

  def format(formatter: ScalaFormatter, result: Result): FormatResult = formatter.format(result)(FormatterState())

  def parse(parser: ScalaParser): Result = parser.expr()

}
