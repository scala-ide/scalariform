package scalariform.formatter

import scalariform.parser.{FullDefOrDcl, ScalaParser}

class MiscFormatterTest extends AbstractFormatterTest {

  """class Foo(
    |  bar: String,
    |  baz: String
    |)""" ==>
  """class Foo(
    |  bar: String,
    |  baz: String)"""

  """class Foo(
    |  bar: String,
    |  baz: String)""" ==>
  """class Foo(
    |  bar: String,
    |  baz: String)"""

  """class Foo(
    |)""" ==>
    """class Foo()"""

  """class a()""" ==>
  """class a()"""

  """def a()""" ==>
  """def a()"""

  """class a(b: Int)""" ==>
  """class a(b: Int)"""

  """def a(b: Int)""" ==>
  """def a(b: Int)"""

  def parse(parser: ScalaParser) = parser.nonLocalDefOrDcl()

  type Result = FullDefOrDcl

  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState(indentLevel = 0))

}
