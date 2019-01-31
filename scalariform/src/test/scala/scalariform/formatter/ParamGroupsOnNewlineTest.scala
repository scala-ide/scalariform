package scalariform.formatter

import scalariform.parser._
import scalariform.formatter.preferences._

// format: OFF
class ParamGroupsOnNewlineTest extends AbstractFormatterTest {

  {
    implicit val formattingPreferences: FormattingPreferences = FormattingPreferences.
      setPreference(AllowParamGroupsOnNewlines, true).
      setPreference(DanglingCloseParenthesis, Force)

  """private[this]
    |def bar
    |[A <: Foo: Bar, B: Baz: Bam]
    |(x: A, y: B)
    |(implicit z: Int)
    |: Foo
    |= {
    |
    |val a = x
    |}""" ==>
  """private[this] def bar
    |  [A <: Foo: Bar, B: Baz: Bam]
    |  (x: A, y: B)
    |  (implicit z: Int): Foo = {
    |
    |  val a = x
    |}"""

    """class Func[T]
    |extends (Int => Int) {
    |
    |val body = ???
    |}""" ==>
  """class Func[T]
    |  extends (Int => Int) {
    |
    |  val body = ???
    |}"""

  """def method
    |(a: A)(b: B)
    |(c: C)
    |(implicit d: D) = {
    |
    |body(a, b, c)
    |}""" ==>
  """def method
    |  (a: A)(b: B)
    |  (c: C)
    |  (implicit d: D) = {
    |
    |  body(a, b, c)
    |}"""

  """class X
    |[T]
    |(a: A)
    |(b: B, c: C)""" ==>
  """class X
    |  [T]
    |  (a: A)
    |  (b: B, c: C)"""


  """object InlineComment {
    |
    |def add
    |(x: Int)
    |// comment
    |(y: Int) = x + y
    |}""" ==>
  """object InlineComment {
    |
    |  def add
    |    (x: Int)
    |    // comment
    |    (y: Int) = x + y
    |}"""

  """class PreserveInline (a: A)(b: B)""" ==>
  """class PreserveInline(a: A)(b: B)"""

  // ensure standalone lambda retains indent level of sibling(s)
  //
  """val lambdaParenFollowsBrace = {
    |val foo = {1}
    |(x: Int) => 42
    |}""" ==>
  """val lambdaParenFollowsBrace = {
    |  val foo = { 1 }
    |  (x: Int) => 42
    |}"""

  """val lambdaParenFollowsParen = {
    |val foo = (1)
    |(x: Int) => 42
    |}""" ==>
  """val lambdaParenFollowsParen = {
    |  val foo = (1)
    |  (x: Int) => 42
    |}"""

  """val lambdaParenFollowsMethodParen = {
    |println("foo")
    |(x: Int) => 42
    |}""" ==>
  """val lambdaParenFollowsMethodParen = {
    |  println("foo")
    |  (x: Int) => 42
    |}"""

  """class Outer[T]
    |(a: A)
    |(b: B, c: C) {
    |
    |class InnerA
    |(a: A)
    |(b: B, c: C) {
    |
    |class InnerA1
    |(a: A)
    |(b: B, c: C)
    |}
    |
    |class InnerB
    |(a: A)
    |(b: B, c: C) {
    |
    |class InnerB1
    |(a: A)
    |(b: B, c: C)
    |}
    |}""" ==>
  """class Outer[T]
    |  (a: A)
    |  (b: B, c: C) {
    |
    |  class InnerA
    |    (a: A)
    |    (b: B, c: C) {
    |
    |    class InnerA1
    |      (a: A)
    |      (b: B, c: C)
    |  }
    |
    |  class InnerB
    |    (a: A)
    |    (b: B, c: C) {
    |
    |    class InnerB1
    |      (a: A)
    |      (b: B, c: C)
    |  }
    |}"""
  }

//  format: ON

  override val debug = false

  def parse(parser: ScalaParser): Result = parser.nonLocalDefOrDcl()

  type Result = FullDefOrDcl

  def format(formatter: ScalaFormatter, result: Result): FormatResult =
    formatter.format(result)(FormatterState(indentLevel = 0))

}
