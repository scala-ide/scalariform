package scalariform.formatter

import scalariform.parser._
import scalariform.formatter.preferences._

// format: OFF
class DoubleIndentMethodFormatterTest extends AbstractFormatterTest {

  override val debug = false

  {

  implicit val formattingPreferences =
    FormattingPreferences.setPreference(DoubleIndentMethodDeclaration, true)

  // Test basic formatting for regressions.
  "def foo" ==> "def foo"

  "def foo=42" ==> "def foo = 42"

  "def foo()" ==> "def foo()"

  "def foo(n:Int)" ==> "def foo(n: Int)"

  """def foo(n: Int
  |)""" ==>
  "def foo(n: Int)"

  "def foo( n:Int,m:String )" ==> "def foo(n: Int, m: String)"

  "def foo{doStuff()}" ==> "def foo { doStuff() }"

  "def foo(x_ : Int)" ==> "def foo(x_ : Int)"

  """def a
    |: String""" ==>
  """def a: String"""

 """def a()
    |{
    |b
    |  }""" ==>
  """def a() {
    |  b
    |}"""

  """def a[B,
    |C]
    |()""" ==>
  """def a[B, C]()"""

  // Verify that continued parameters are maintained & doubly-indented.
  """def foo(
    |  a: String)""" ==>
  """def foo(
    |    a: String)"""

  """def foo(a: String,
    |  b: String)""" ==>
  """def foo(
    |    a: String,
    |    b: String)"""

  """def foo(a: String,
    |      b: String
    |)""" ==>
  """def foo(
    |    a: String,
    |    b: String)"""

  """def foo(a: String, b: String,
    |    c: String)""" ==>
  """def foo(a: String, b: String,
    |    c: String)"""

  """def foo(
    |    a: String,
    |      b: String)""" ==>
  """def foo(
    |    a: String,
    |    b: String)"""
  }

  def parse(parser: ScalaParser) = parser.nonLocalDefOrDcl()

  type Result = FullDefOrDcl

  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState(indentLevel = 0))
}
