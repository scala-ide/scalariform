package scalariform.formatter

import scalariform.parser._

// format: OFF
class MutateTest extends AbstractFormatterTest {

  // avoid gapless assignment

  """object Test {
    |  htmlNodeList.foreach(_.outerHTML=block.toString)
    |}""" ==>
  """object Test {
    |  htmlNodeList.foreach(_.outerHTML = block.toString)
    |}"""

  """object Test {
    |  maybeHtmlNode.foreach(_.outerHTML= block.toString)
    |}""" ==>
  """object Test {
    |  maybeHtmlNode.foreach(_.outerHTML = block.toString)
    |}"""

  """val assignment = foo.foreach(bar.baz = _)""" ==>
  """val assignment = foo.foreach(bar.baz = _)"""

//  format: ON

  override val debug = false

  def parse(parser: ScalaParser): Result = parser.nonLocalDefOrDcl()

  type Result = FullDefOrDcl

  def format(formatter: ScalaFormatter, result: Result): FormatResult =
    formatter.format(result)(FormatterState(indentLevel = 0))

}
