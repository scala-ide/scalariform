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

  """a match {
    |  case b(c @ ~()) =>
    |  case b(c@ ~()) =>
    |}""" ==>
  """a match {
    |  case b(c@ ~()) =>
    |  case b(c@ ~()) =>
    |}"""


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

}
