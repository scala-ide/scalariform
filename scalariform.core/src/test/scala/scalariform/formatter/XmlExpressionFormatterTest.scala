package scalariform.formatter

import scalariform.parser._
import scalariform.formatter._
import scalariform.formatter.preferences._

// format: OFF
class XmlExpressionFormatterTest extends AbstractExpressionFormatterTest {

  {
    implicit val formattingPreferences = FormattingPreferences.setPreference(FormatXml, true)
    "<a b = 'c'/>" ==> "<a b='c'/>"
  }

  """b(<c d={e + 
    |"f"}/>)""" ==>
  """b(<c d={
    |  e +
    |    "f"
    |}/>)"""

  """b(<c d={e + 
    |"f"}></c>)""" ==>
  """b(<c d={
    |  e +
    |    "f"
    |}></c>)"""


  override val debug = false
  
}
