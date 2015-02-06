package com.danieltrinh.scalariform.formatter

import com.danieltrinh.scalariform.parser._

abstract class AbstractExpressionFormatterTest extends AbstractFormatterTest {

  type Result = Expr

  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState())

  def parse(parser: ScalaParser) = parser.expr()

}
