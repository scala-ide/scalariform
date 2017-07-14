package scalariform.formatter

import scalariform.formatter.preferences._

class ParenAndBracketSpacingTest extends AbstractExpressionFormatterTest {

  {
    implicit val formattingPreferences = FormattingPreferences.setPreference(SpaceInsideParentheses, true)
    "()" ==> "()"
    "(a: Int) => 3" ==> "( a: Int ) => 3"
    "(3)" ==> "( 3 )"
    "(3, 4)" ==> "( 3, 4 )"
    "for (n <- 1 to 10) yield foo(n, n)" ==> "for ( n <- 1 to 10 ) yield foo( n, n )"
  }

  {
    implicit val formattingPreferences = FormattingPreferences.setPreference(SpaceInsideBrackets, true)
    "x: List[String]" ==> "x: List[ String ]"
    "foo[Bar](baz)" ==> "foo[ Bar ](baz)"
    "{ class A[B] { private[this] val bob } }" ==> "{ class A[ B ] { private[ this ] val bob } }"
    "super[X].y" ==> "super[ X ].y"
    "foo[Bar](baz)[Biz]" ==> "foo[ Bar ](baz)[ Biz ]"
    "foo[Bar][Baz][Buz]" ==> "foo[ Bar ][ Baz ][ Buz ]"

    """foo(
      |alpha = "foo",
      |beta = bar match {
      |  case _ => "bar"
      |},
      |gamma = false
      |)""" ==>
    """foo(
      |  alpha = "foo",
      |  beta = bar match {
      |    case _ => "bar"
      |  },
      |  gamma = false)"""

    """foo(
      |alpha = "foo",
      |beta = bar(
      |a = 1
      |),
      |gamma = false
      |)""" ==>
    """foo(
      |  alpha = "foo",
      |  beta = bar(
      |    a = 1),
      |  gamma = false)"""

    """foo(
      |arg = bar(
      |baz = "a"
      |).xyz
      |)""" ==>
    """foo(
      |  arg = bar(
      |    baz = "a").xyz)"""
  }

  {
    implicit val formattingPreferences = FormattingPreferences.setPreference(DanglingCloseParenthesis, Force)
    """foo(
      |alpha = "foo",
      |beta = "bar",
      |gamma = false)""" ==>
    """foo(
      |  alpha = "foo",
      |  beta = "bar",
      |  gamma = false
      |)"""

    """foo(
      |"foo",
      |"bar",
      |false)""" ==>
    """foo(
      |  "foo",
      |  "bar",
      |  false
      |)"""

    // TODO find out why parens based sub expressions do not dangle

    """foo(
      |alpha = "foo",
      |beta = collection.map { x =>
      |  x
      |}
      )""" ==>
    """foo(
      |  alpha = "foo",
      |  beta = collection.map { x =>
      |    x
      |  }
      |)"""
  }

  {
    implicit val formattingPreferences = FormattingPreferences.setPreference(DanglingCloseParenthesis, Preserve)
    """foo(
      |alpha = "foo",
      |beta = "bar",
      |gamma = false)""" ==>
    """foo(
      |  alpha = "foo",
      |  beta = "bar",
      |  gamma = false)"""

    """foo(
      |alpha = "foo",
      |beta = "bar",
      |gamma = false
      |)""" ==>
    """foo(
      |  alpha = "foo",
      |  beta = "bar",
      |  gamma = false
      |)"""

    """foo(
      |"foo",
      |"bar",
      |false)""" ==>
    """foo(
      |  "foo",
      |  "bar",
      |  false)"""

    """foo(
      |"foo",
      |"bar",
      |false
      |)""" ==>
    """foo(
      |  "foo",
      |  "bar",
      |  false
      |)"""
  }

  {
    implicit val formattingPreferences = FormattingPreferences.setPreference(DanglingCloseParenthesis, Prevent)
    """foo(
      |alpha = "foo",
      |beta = "bar",
      |gamma = false)""" ==>
    """foo(
      |  alpha = "foo",
      |  beta = "bar",
      |  gamma = false)"""

    """foo(
      |alpha = "foo",
      |beta = "bar",
      |gamma = false
      |)""" ==>
    """foo(
      |  alpha = "foo",
      |  beta = "bar",
      |  gamma = false)"""

    """foo(
      |"foo",
      |"bar",
      |false)""" ==>
    """foo(
      |  "foo",
      |  "bar",
      |  false)"""

    """foo(
      |"foo",
      |"bar",
      |false
      |)""" ==>
    """foo(
      |  "foo",
      |  "bar",
      |  false)"""

    """foo(
      |alpha = "foo",
      |beta = "bar",
      |gamma = false // comment
      |)""" ==>
    """foo(
      |  alpha = "foo",
      |  beta = "bar",
      |  gamma = false // comment
      |)"""
  }

}
