
package scalariform.formatter

import scalariform.parser._
import scalariform.formatter.preferences._

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

  "def foo[X<%T1,Y<:T2,Z>:T3]()" ==> "def foo[X <% T1, Y <: T2, Z >: T3]()"

  "def foo(x: =>Int, y: Int *)" ==> "def foo(x: => Int, y: Int*)"

  "def a[+[+_]]" ==> "def a[+[+_]]"
  "def a[+]" ==> "def a[+]"
  "def a[-]" ==> "def a[-]"

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
    |   1
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

  """class Foo[A : B]() {}""" ==>
  """class Foo[A: B]() {}"""

  """def foo[A : B]()""" ==>
  """def foo[A: B]()"""

  """def foo[A: B :C]()""" ==>
  """def foo[A: B: C]()"""

  {
  // SpaceBeforeColon should add a space for the context colon (for backwards-compatibility).
  implicit val formattingPreferences =
    FormattingPreferences.setPreference(SpaceBeforeColon, true).setPreference(SpaceBeforeContextColon, false)

  """class Foo[A : B]() {}""" ==>
  """class Foo[A : B]() {}"""
  }

  {
  // Typeclass formatting should not be affected by the SpaceBeforeColon setting.
  implicit val formattingPreferences =
    FormattingPreferences.setPreference(SpaceBeforeColon, false).setPreference(SpaceBeforeContextColon, true)

  """class Foo[A : B]() {}""" ==>
  """class Foo[A : B]() {}"""

  """def foo[A : B]()""" ==>
  """def foo[A : B]()"""

  """def foo[A: B :C]()""" ==>
  """def foo[A : B : C]()"""

  }
  {
  // Typeclass formatting should not be affected by the SpaceBeforeColon setting.
  implicit val formattingPreferences =
    FormattingPreferences.setPreference(SpaceBeforeColon, true).setPreference(SpaceBeforeContextColon, true)

  """class Foo[A : B]() {}""" ==>
  """class Foo[A : B]() {}"""

  """def foo[A : B]()""" ==>
  """def foo[A : B]()"""

  """def foo[A: B :C]()""" ==>
  """def foo[A : B : C]()"""

  }

  {

  implicit val formattingPreferences =
    FormattingPreferences
      .setPreference(IndentLocalDefs, true)

  """class A {
    |  def b() = {
    |    def c() = 42
    |    println("d")
    |    def e() =
    |    42
    |println("f")
    |    def g() {
    |println("h")
    |}
    |    c() * e()
    |  }
    |}""" ==>
  """class A {
    |  def b() = {
    |      def c() = 42
    |    println("d")
    |      def e() =
    |        42
    |    println("f")
    |      def g() {
    |        println("h")
    |      }
    |    c() * e()
    |  }
    |}"""

  """val x = { i =>
    |  def a
    |  def b
    |}""" ==>
  """val x = { i =>
    |    def a
    |    def b
    |}"""

  // See issue #24
  """val inc: Int => Int = { i =>
    |  def plus1 = i + 1
    |  plus1
    |}""" ==>
  """val inc: Int => Int = { i =>
    |    def plus1 = i + 1
    |  plus1
    |}"""

  }

  {
    implicit val formattingPreferences = FormattingPreferences.setPreference(DanglingCloseParenthesis, Preserve)

    """def foo(
      |  alpha: Int,
      |  beta: String = "default",
      |  gamma: Boolean = true,
      |  delta: Boolean = false): Double = {
      |
      |  bar
      |}""" ==>
    """def foo(
      |  alpha: Int,
      |  beta: String = "default",
      |  gamma: Boolean = true,
      |  delta: Boolean = false): Double = {
      |
      |  bar
      |}"""

    """def foo(
      |  alpha: Int,
      |  beta: String = "default",
      |  gamma: Boolean = true,
      |  delta: Boolean = false
      |): Double = {
      |
      |  bar
      |}""" ==>
    """def foo(
      |  alpha: Int,
      |  beta: String = "default",
      |  gamma: Boolean = true,
      |  delta: Boolean = false
      |): Double = {
      |
      |  bar
      |}"""
  }

  {
    implicit val formattingPreferences = FormattingPreferences.setPreference(DanglingCloseParenthesis, Prevent)

    """private def foo(
      |  alpha: Int,
      |  beta: String = "default",
      |  gamma: Boolean = true,
      |  delta: Boolean = false
      |): Double = {
      |
      |  bar
      |}""" ==>
    """private def foo(
      |  alpha: Int,
      |  beta: String = "default",
      |  gamma: Boolean = true,
      |  delta: Boolean = false): Double = {
      |
      |  bar
      |}"""

    """case class FqnSymbol(
      |  id: Option[Int],
      |  file: String, // the underlying file
      |  path: String, // the VFS handle (e.g. classes in jars)
      |  fqn: String,
      |  descriptor: Option[String], // for methods
      |  internal: Option[String], // for fields
      |  source: Option[String], // VFS
      |  line: Option[Int],
      |  offset: Option[Int] = None // future features:
      |)""" ==>
    """case class FqnSymbol(
      |  id: Option[Int],
      |  file: String, // the underlying file
      |  path: String, // the VFS handle (e.g. classes in jars)
      |  fqn: String,
      |  descriptor: Option[String], // for methods
      |  internal: Option[String], // for fields
      |  source: Option[String], // VFS
      |  line: Option[Int],
      |  offset: Option[Int] = None // future features:
      |)"""
  }

  {
    implicit val formattingPreferences = FormattingPreferences.setPreference(DanglingCloseParenthesis, Force)

    """private def foo(
      |  alpha: Int,
      |  beta: String = "default",
      |  gamma: Boolean = true,
      |  delta: Boolean = false): Double = {
      |
      |  bar
      |}""" ==>
    """private def foo(
      |  alpha: Int,
      |  beta: String = "default",
      |  gamma: Boolean = true,
      |  delta: Boolean = false
      |): Double = {
      |
      |  bar
      |}"""
  }

  """def foo(n, m)""" ==>
  """def foo(n, m)"""

  """def test(test: ^^ *)""" ==>
  """def test(test: ^^ *)"""

  def parse(parser: ScalaParser) = parser.nonLocalDefOrDcl()

  type Result = FullDefOrDcl

  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState(indentLevel = 0))

}
