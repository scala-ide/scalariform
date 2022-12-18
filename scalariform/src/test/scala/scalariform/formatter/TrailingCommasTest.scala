package scalariform.formatter

import scalariform.parser._

// format: OFF
class TrailingCommasTest extends AbstractFormatterTest {
  // Trailing comma must always be followed by a newline.
  // Scalariform will insert a newline if it is missing, to make
  // such code valid Scala.

  // The test cases with List() cover trailing commas in function invocations.

  "val x = List()" ==> "val x = List()"
  "val x = List(1)" ==> "val x = List(1)"
  "val x = List(1, 2)" ==> "val x = List(1, 2)"

  "val x = List(1,)" ==>
    """val x = List(1,
      |)"""

  """val x = List(
    |  1
    |)""" ==>
    """val x = List(
      |  1)"""

  """val x = List(
    |  1,
    |)""" ==>
    """val x = List(
      |  1,
      |)"""

  "val x = List(1, 2,)" ==>
    """val x = List(1, 2,
      |)"""

  """val x = List(
    |  1, 2
    |)""" ==>
    """val x = List(
      |  1, 2)"""

  """val x = List(
    |  1, 2,
    |)""" ==>
    """val x = List(
      |  1, 2,
      |)"""

  """val x = List(
    |  1,
    |  2,
    |)""" ==>
    """val x = List(
      |  1,
      |  2,
      |)"""

  """val x = List(
    |  1,
    |  2
    |)""" ==>
    """val x = List(
      |  1,
      |  2)"""

  """val x = List(
    |  1,
    |  2,)""" ==>
    """val x = List(
      |  1,
      |  2,
      |)"""

  // The test cases with case class cover trailing commas in parameter declarations.

  "case class A" ==> "case class A"
  "case class A()" ==> "case class A()"
  "case class A(x: Int)" ==> "case class A(x: Int)"
  "case class A(x: Int,)" ==>
    """case class A(
      |  x: Int,
      |)"""

  "case class A(x: Int, y: Int)" ==> "case class A(x: Int, y: Int)"
  "case class A(x: Int, y: Int,)" ==>
    """case class A(
      |  x: Int, y: Int,
      |)"""

  """case class A(
    |  x: Int,
    |  y: Int,
    |)""" ==>
    """case class A(
      |  x: Int,
      |  y: Int,
      |)"""

  """case class A(
    |  x: Int,
    |  y: Int,)""" ==>
    """case class A(
      |  x: Int,
      |  y: Int,
      |)"""

  """case class A(
    |  x: Int, y: Int,
    |)""" ==>
    """case class A(
      |  x: Int, y: Int,
      |)"""

  """case class A(
    |  x: Int, y: Int,)""" ==>
    """case class A(
      |  x: Int, y: Int,
      |)"""

  //  format: ON

  override val debug = true

  def parse(parser: ScalaParser) = parser.nonLocalDefOrDcl()

  type Result = FullDefOrDcl

  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState(indentLevel = 0))

}
