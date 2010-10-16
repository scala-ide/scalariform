package scalariform.formatter

import scalariform.parser._
import scalariform.formatter._

// format: OFF
class DefOrDclFormatterTest extends AbstractFormatterTest {

  override val debug = false

  "def foo" ==> "def foo"

  "def foo()" ==> "def foo()"

  "def foo=42" ==> "def foo = 42"

  "def foo{doStuff()}" ==> "def foo { doStuff() }"

  "def foo={doStuff()}" ==> "def foo = { doStuff() }"

  "def foo ()" ==> "def foo()"
 
  "def foo(n:Int)" ==> "def foo(n: Int)"

  "def foo( n:Int,m:String )" ==> "def foo(n: Int, m: String)"

  "def foo:Int" ==> "def foo: Int"

  "def modN ( n :Int ) (x: Int) = ((x % n) == 0)" ==> "def modN(n: Int)(x: Int) = ((x % n) == 0)"

  "def foo(a: Int=123+2, b: Int=456+7)" ==> "def foo(a: Int = 123 + 2, b: Int = 456 + 7)"
  
  "def foo [ - T , + U ] (t: T): U" ==> "def foo[-T, +U](t: T): U"

  "def foo[X<%T1,Y<:T2,Z>:T3]()" ==> "def foo[X <% T1, Y <: T2, Z >: T3]()"

  "def foo(x: =>Int, y: Int *)" ==> "def foo(x: => Int, y: Int*)"

  "def foo(x_ : Int)" ==> "def foo(x_ : Int)"

  "private def x" ==> "private def x"

  "private[pack]def x" ==> "private[pack] def x"
  "private[pack]val x" ==> "private[pack] val x"
  "private[pack]var x" ==> "private[pack] var x"

  """def a
    |: String""" ==>
  """def a: String"""

  """def foo =
    |    if (true)
    | 	1
    |    else
    |   2""" ==>
  """def foo =
    |  if (true)
    |    1
    |  else
    |    2"""

  """val x =
    |  if (true)
    |    42
    |  else
    |    24""" ==>
  """val x =
    |  if (true)
    |    42
    |  else
    |    24"""

 """def a()
    |{
    |b
    |  }""" ==>
  """def a() {
    |  b
    |}"""

  """type
    |B""" ==>
  """type B"""

  """type
    |
    |B""" ==>
  """type B"""

  "class A { val b = _.c }" ==> "class A { val b = _.c }"

  """def a() { return }""" ==>  """def a() { return }"""

  """def a[B,
    |C]
    |()""" ==>
  """def a[B, C]()"""

  """private [a]
    |sealed trait B""" ==>
  """private[a] sealed trait B"""

  def parse(parser: ScalaParser) = parser.nonLocalDefOrDcl()

  type Result = FullDefOrDcl
  
  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState(indentLevel = 0))


}

 
