package scalariform.formatter

import scalariform.parser._
import scalariform.formatter._

// format: OFF
class BlockExprFormatterTest extends AbstractExpressionFormatterTest {

  override val debug = false

  """{ 
    |  a();
    |  b()
    |}""" ==>
  """{
    |  a();
    |  b()
    |}"""

  """{
    |a;b;c;
    |d;e;f//Foo
    |g/*h*/;i
    |/* j */
    |k
    |}""" ==>
  """{
    |  a; b; c;
    |  d; e; f //Foo
    |  g /*h*/ ; i
    |  /* j */
    |  k
    |}"""

  """{
    |val x = { 
    |a()
    |b()
    |}
    |}""" ==>
  """{
    |  val x = {
    |    a()
    |    b()
    |  }
    |}"""

  """{
    |1 +
    |2
    |}""" ==>
  """{
    |  1 +
    |    2
    |}"""

  "{ object A }" ==> "{ object A }"

  "{ class A }" ==> "{ class A }"

  """{ 
    |class A
    |class B
    |}""" ==>
  """{
    |  class A
    |  class B
    |}"""
  
  "{ case 42 => }" ==> "{ case 42 => }"
  "{ case -42 => }" ==> "{ case -42 => }"

  """{
    |  println("foo")
    |  (x: Int) => 42
    |}""" ==>
  """{
    |  println("foo")
    |  (x: Int) => 42
    |}"""
 
  """{
    |c !
    |val b
    |}""" ==>
  """{
    |  c !
    |  val b
    |}"""

  """{
    |  if (b) { /**/}
    |  false
    |}""" ==>
  """{
    |  if (b) { /**/ }
    |  false
    |}"""

  """{
    |}""" ==>
  """{
    |}"""

  """{
    |
    |}""" ==>
  """{
    |
    |}"""

  "{ ; a }" ==> "{ ; a }"

}
