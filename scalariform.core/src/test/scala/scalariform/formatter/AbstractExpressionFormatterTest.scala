package scalariform.formatter

import scalariform.parser._

abstract class AbstractExpressionFormatterTest extends AbstractFormatterTest {

  type Result = Expr

  def getParser(parser: ScalaCombinatorParser): ScalaCombinatorParser#Parser[Result] = parser.phrase(parser.expr)

  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState())

}
