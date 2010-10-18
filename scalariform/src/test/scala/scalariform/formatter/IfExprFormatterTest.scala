package scalariform.formatter

import scalariform.parser._
import scalariform.formatter._

class IfExprFormatterTest extends AbstractExpressionFormatterTest {

  override val debug = false

  // format: OFF

  "if(x>y)(x)else(y)" ==> "if (x > y) (x) else (y)"

  "if (true) 3 else 4" ==> "if (true) 3 else 4" 

  """if (true)
    |println("Hello world")""" ==>
  """if (true)
    |  println("Hello world")"""

  """if (true) // Comment
    |println("Hello world")""" ==>
  """if (true) // Comment
    |  println("Hello world")"""

  """if (1 == 1)
    |println("wibble")
    |else
    |println("wobble")""" ==>
  """if (1 == 1)
    |  println("wibble")
    |else
    |  println("wobble")"""

  """if (1 == 1)
    |println("wibble")
    |else if (1 == 2)
    |println("wobble")
    |else
    |println("wobble")""" ==>
  """if (1 == 1)
    |  println("wibble")
    |else if (1 == 2)
    |  println("wobble")
    |else
    |  println("wobble")""" 

  """if (1 == 1)   println("wibble")
    |  else println("wobble")""" ==>
  """if (1 == 1) println("wibble")
    |else println("wobble")"""

  """ if  (1==1)  println("wibble") """ ==> """if (1 == 1) println("wibble")"""

  "if(x>y){x}else{y}" ==> "if (x > y) { x } else { y }"

  """if (x > y) {
    |println("Foo") }""" ==> 
  """if (x > y) {
    |  println("Foo")
    |}"""
 
  """if (x > y) {
    |println("Foo") } else { 
    |println("Bar") }""" ==> 
  """if (x > y) {
    |  println("Foo")
    |} else {
    |  println("Bar")
    |}"""

  """if (x > y) {
    |println("Foo")
    |println("Bar")}""" ==> 
  """if (x > y) {
    |  println("Foo")
    |  println("Bar")
    |}"""


  """if (1 == 2) {
    |println("bob")
    |println("bob")
    |if (2 == 3) {
    |println("fred")
    |println("fred")
    |} else {
    |if (3 == 4) {
    |println("bob")
    |println("bob")
    |} else if (4 == 5) {
    |println("fred")
    |println("fred")
    |}
    |println("bob")
    |}
    |}""" ==>
  """if (1 == 2) {
    |  println("bob")
    |  println("bob")
    |  if (2 == 3) {
    |    println("fred")
    |    println("fred")
    |  } else {
    |    if (3 == 4) {
    |      println("bob")
    |      println("bob")
    |    } else if (4 == 5) {
    |      println("fred")
    |      println("fred")
    |    }
    |    println("bob")
    |  }
    |}"""

  """if (1 == 2) { println("bob")
    |}""" ==>
  """if (1 == 2) {
    |  println("bob")
    |}"""

  """if (1 == 2) { println("bob")
    |println("fred")
    |}""" ==>
  """if (1 == 2) {
    |  println("bob")
    |  println("fred")
    |}"""

  """if (true) {
    |   println("wobble") } 
    |else { 
    |println("wobble") }""" ==>
  """if (true) {
    |  println("wobble")
    |} else {
    |  println("wobble")
    |}"""

  """if (true){}""" ==>
  """if (true) {}"""

  """if (true){
    |}""" ==>
  """if (true) {
    |}"""

  "if (true){;}" ==> "if (true) { ; }"

  """if (1 == 2) {println("wibble");println("wobble")}""" ==>
  """if (1 == 2) { println("wibble"); println("wobble") }"""

  """if (true) 
    |{ x }""" ==>
  """if (true) { x }"""

  """if (a)
    |if (b) 
    |x 
    |else 
    |y""" ==>
  """if (a)
    |  if (b)
    |    x
    |  else
    |    y"""

  """if (true) a
    |else
    |if (true)
    |b""" ==>
  """if (true) a
    |else if (true)
    |  b"""

  """if /*a*/ ( /*b*/ true /*c*/ ) /*d*/ {
    |  1
    |} /*e*/ else /*f*/ {
    |  2
    |}""" ==>
  """if /*a*/ ( /*b*/ true /*c*/ ) /*d*/ {
    |  1
    |} /*e*/ else /*f*/ {
    |  2
    |}"""

  """if (true) 1 else 
    |{ 2 } """ ==>
  """if (true) 1 else { 2 }"""

  """if (true) { 1 
    |}
    |else 2""" ==>
  """if (true) {
    |  1
    |} else 2"""

  """if (true) 1 else 
    |{ 
    |2 }""" ==>
  """if (true) 1 else {
    |  2
    |}"""

  """if (true) { 
    | 1 
    |} + 2
    | else  {
    |2 
    |} + 2""" ==>
  """if (true) {
    |  1
    |} + 2
    |else {
    |  2
    |} + 2"""

  """if (condition) // comment
    |1
    |else // comment
    |2""" ==>
  """if (condition) // comment
    |  1
    |else // comment
    |  2"""

  """if (c)// comment
    |  1
    |else// comment
    |  2""" ==>
  """if (c) // comment
    |  1
    |else // comment
    |  2"""

  """if (true) // Foo
    |{}  else // Bar
    |{}""" ==>
  """if (true) // Foo
    |{} else // Bar
    |{}"""

  """if//a
    | (//b
    |true//c
    |)//d
    |{}//e
    |else//f
    |{}""" ==>
  """if //a
    |( //b
    |true //c
    |) //d
    |{} //e
    |else //f
    |{}"""

  """if (true)
    |for {
    | y <- ys}yield {
    |1} else {
    |2
    |}""" ==>
  """if (true)
    |  for {
    |    y <- ys
    |  } yield {
    |    1
    |  }
    |else {
    |  2
    |}"""

  """if (true) 1;
    |else 2""" ==>
  """if (true) 1;
    |else 2"""

  """if (b)
    |  c
    |  { d }
    |else 
    |  e""" ==>
  """if (b)
    |  c { d }
    |else
    |  e"""

  """if (a) {}
    |else b""" ==>
  """if (a) {}
    |else b"""

  """if (a) {}
    |else {}""" ==>
  """if (a) {}
    |else {}"""

  """if (a) {println()}
    |else {println()}""" ==>
  """if (a) { println() }
    |else { println() }"""

  """Some(if (a) 
    |b 
    |else 
    |c)""" ==>
  """Some(if (a)
    |  b
    |else
    |  c)"""

  """Some(if (a) {
    |b 
    |} else 
    |c)""" ==>
  """Some(if (a) {
    |  b
    |} else
    |  c)"""

  """Some(if (a) { b } else 
    |c)""" ==>
  """Some(if (a) { b } else
    |  c)"""

  """if (cond) 
    |42 else
    | 42""" ==>
  """if (cond)
    |  42
    |else
    |  42"""
    
  """if (a)
    |b else {
    |c
    |}""" ==>
  """if (a)
    |  b
    |else {
    |  c
    |}"""

  """if (a)
    |b else if (c) {
    |d} else e""" ==>
  """if (a)
    |  b
    |else if (c) {
    |  d
    |} else e"""

}
 
