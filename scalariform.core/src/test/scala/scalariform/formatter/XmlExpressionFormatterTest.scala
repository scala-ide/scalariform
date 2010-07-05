package scalariform.formatter

import scalariform.parser._
import scalariform.formatter._
import scalariform.formatter.preferences._

// format: OFF
class XmlExpressionFormatterTest extends AbstractExpressionFormatterTest {

    "<a b = 'c'/>" ==> "<a b='c'/>"

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

  "<a>1</a>" ==> "<a>1</a>"
  "<a> 1 </a>" ==> "<a>1</a>"

  """<a>
    |1</a>""" ==>
  """<a>
    |  1
    |</a>"""

  """<a><b>1</b>
    |<b>2</b>
    |<b>3</b>
    |</a>""" ==>
  """<a>
    |  <b>1</b>
    |  <b>2</b>
    |  <b>3</b>
    |</a>"""

  """{
    |<html>{
    |println("Foo")
    |}</html>
    |}""" ==>
  """{
    |  <html>{
    |    println("Foo")
    |  }</html>
    |}"""
    
  """{
    |    <package>
    |    <name>{ name.get }</name>
    |    <version>{ version.get }</version></package>
    |}""" ==>    
  """{
    |  <package>
    |    <name>{ name.get }</name>
    |    <version>{ version.get }</version>
    |  </package>
    |}"""

  """class A {
    |val b = <c>
    |<d/></c>
    |}""" ==>
  """class A {
    |  val b = <c>
    |            <d/>
    |          </c>
    |}"""

  override val debug = false

  // implicit val formattingPreferences = FormattingPreferences.setPreference(FormatXml, true)

}
