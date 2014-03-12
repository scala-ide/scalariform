package scalariform.formatter

import scalariform.parser._
import scalariform.formatter._
import scalariform.formatter.preferences._

// format: OFF
class CaseClausesFormatterTest extends AbstractExpressionFormatterTest {

  override val debug = false

  """{
    |  case x => x * x
    |  case y => y * 2
    |}""" ==>
  """{
    |  case x => x * x
    |  case y => y * 2
    |}"""

  """{
    |case 1 => // comment
    |2
    |}""" ==>
  """{
    |  case 1 => // comment
    |    2
    |}"""

  """{
    |case x =>
    |while (true) {
    |1
    |}
    |}""" ==>
  """{
    |  case x =>
    |    while (true) {
    |      1
    |    }
    |}"""

  """a match {
    |    case b => c();
    |d()
    |  }""" ==>
  """a match {
    |  case b =>
    |    c();
    |    d()
    |}"""

  """a match {
    |case b => {
    |c
    |}
    |}""" ==>
  """a match {
    |  case b => {
    |    c
    |  }
    |}"""

  """a match {
    |case b => {
    |c
    |}
    |d
    |}""" ==>
  """a match {
    |  case b =>
    |    {
    |      c
    |    }
    |    d
    |}"""

  """a match { case a => b; c; d }""" ==>
  """a match { case a => b; c; d }"""

  """a match { case a => b; c;
    |d }""" ==>
  """a match {
    |  case a =>
    |    b; c;
    |    d
    |}"""

  "a match { case b => ; c }" ==> "a match { case b => ; c }"

  // See issue #60
  """a match {
    |case b =>
    |val c = d
    |case e =>
    |}""" ==>
  """a match {
    |  case b =>
    |    val c = d
    |  case e =>
    |}"""

  """a match {
    |/* foo*/
    |case x if z=> 1
    |/* bar*/
    |case yy => 2
    |/* baz*/
    |case zzz => 3
    |}""" ==>
  """a match {
    |  /* foo*/
    |  case x if z => 1
    |  /* bar*/
    |  case yy => 2
    |  /* baz*/
    |  case zzz => 3
    |}"""

  {
  implicit val formattingPreferences = FormattingPreferences.setPreference(SpacesWithinPatternBinders, false)

  """a match {
    |  case b(c @ ~()) =>
    |  case b(c@ ~()) =>
    |}""" ==>
  """a match {
    |  case b(c@ ~()) =>
    |  case b(c@ ~()) =>
    |}"""
  }

  {

  implicit val formattingPreferences = FormattingPreferences.setPreference(AlignSingleLineCaseStatements, true)

  """a match {
    |case x => 1
    |case yy => 2
    |case zzz => 3
    |}""" ==>
  """a match {
    |  case x   => 1
    |  case yy  => 2
    |  case zzz => 3
    |}"""

  """a match {
    |/* foo*/
    |case x if z=> 1
    |/* bar*/
    |case yy => 2
    |/* baz*/
    |case zzz => 3
    |}""" ==>
  """a match {
    |  /* foo*/
    |  case x if z => 1
    |  /* bar*/
    |  case yy     => 2
    |  /* baz*/
    |  case zzz    => 3
    |}"""

  """a match {
    |case b => 1; case c => 2
    |}""" ==>
  """a match {
    |  case b => 1; case c => 2
    |}"""

  """a match { case b => 1;
    |case ccc => 2}""" ==>
  """a match {
    |  case b   => 1;
    |  case ccc => 2
    |}"""

  """a match {
    |  case b /* comment */ => 1
    |  case c => 2
    |}""" ==>
  """a match {
    |  case b /* comment */ => 1
    |  case c               => 2
    |}"""

  """a match {
    |  case b
    |=> 1
    |  case ccc => 2
    |}""" ==>
  """a match {
    |  case b   => 1
    |  case ccc => 2
    |}"""

  """a match {
    |  case b =>
    | 1
    |  case cc => 2
    |}""" ==>
  """a match {
    |  case b =>
    |    1
    |  case cc => 2
    |}"""

  "{ case a=> b }" ==> "{ case a => b }"}

  """t match {
    |  case Cell [ a ] (x: Int) => c.x = 5
    |}""" ==>
  """t match {
    |  case Cell[a](x: Int) => c.x = 5
    |}"""


  {
  implicit val formattingPreferences =
    FormattingPreferences.setPreference(AlignSingleLineCaseStatements, true).setPreference(RewriteArrowSymbols, true)

  """a match {
    |case b => 42
    |case ccc => 24
    |}""" ==>
  """a match {
    |  case b   ⇒ 42
    |  case ccc ⇒ 24
    |}"""
  }

  {

  implicit val formattingPreferences =
    FormattingPreferences
      .setPreference(AlignSingleLineCaseStatements, true)
      .setPreference(AlignSingleLineCaseStatements.MaxArrowIndent, 5)

  """x match {
    |  case 123456789 => a
    |  case _ => b
    |}""" ==>
  """x match {
    |  case 123456789 => a
    |  case _ => b
    |}"""

  }

  """a match {
    |  case true => {
    |    case true => false
    |    case false => true
    |  }
    |  case false => b match {
    |    case true => true
    |    case false => false
    |  }
    |}""" ==>
  """a match {
    |  case true => {
    |    case true => false
    |    case false => true
    |  }
    |  case false => b match {
    |    case true => true
    |    case false => false
    |  }
    |}"""


  "{ case a ::(b) ⇒ }" ==>
  "{ case a :: (b) ⇒ }"

  """{
    |  case a ⇒
    |    {
    |      a
    |    }
    |}""" ==>
  """{
    |  case a ⇒
    |    {
    |      a
    |    }
    |}"""


  {
  implicit val formattingPreferences = FormattingPreferences.setPreference(SpacesWithinPatternBinders, false)

  """(a: @switch)  match {
    |case elem@Multi(values@_*) =>
    |}""" ==>
  """(a: @switch) match {
    |  case elem@Multi(values@_*) =>
    |}"""
  }

  """(a: @switch)  match {
    |case elem@Multi(values@_*) =>
    |}""" ==>
  """(a: @switch) match {
    |  case elem @ Multi(values @ _*) =>
    |}"""

}