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

//  format: ON

  override val debug = false

  def parse(parser: ScalaParser) = parser.nonLocalDefOrDcl()

  type Result = FullDefOrDcl

  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState(indentLevel = 0))

}
