package scalariform.formatter

import scalariform.parser._
import scalariform.formatter._

// format: OFF
class ForExprFormatterTest extends AbstractExpressionFormatterTest {

//  override val debug = true

  "for (x <- xs) yield x" ==> "for (x <- xs) yield x"

  "for (x <- xs) println(x)" ==> "for (x <- xs) println(x)"

  """for (x <- 1 to 10)
    |  println("hello")""" ==>
  """for (x <- 1 to 10)
    |  println("hello")"""

  """for (x <- 1 to 10) {
    |  println("hello")
    |}""" ==>
  """for (x <- 1 to 10) {
    |  println("hello")
    |}"""

  """for { x <- xs
    |      y <- ys
    |      if x > y } yield x + y""" ==>
  """for {
    |  x <- xs
    |  y <- ys
    |  if x > y
    |} yield x + y"""

  "for (x <- xs) a()" ==> "for (x <- xs) a()"

  """for (x <- xs) 
    |a()""" ==>
  """for (x <- xs)
    |  a()"""

  """for(x <- xs)
    |{
    |a()
    |}""" ==>
  """for (x <- xs) {
    |  a()
    |}"""

  """for(x <- xs) { a() }""" ==>
  """for (x <- xs) { a() }"""

  """for(x <- xs) 
    |{ a() }""" ==>
  """for (x <- xs) { a() }"""

  """for(x <- xs) yield
    |{ 
    |  a + b
    |}""" ==>
  """for (x <- xs) yield {
    |  a + b
    |}"""

  "for(x <- xs) yield 3" ==> "for (x <- xs) yield 3"

  "for(x <- xs) yield { 3 }" ==> "for (x <- xs) yield { 3 }"

  """for(x <- xs) yield
    |{ a + b
    |}""" ==>
  """for (x <- xs) yield {
    |  a + b
    |}"""

  """for(x <- xs)
    |yield
    |{ 
    |z } """ ==>
  """for (x <- xs) yield {
    |  z
    |}"""

  "for{x<-xs if(true)}yield( if (true) 1 else 2)" ==> "for { x <- xs if (true) } yield (if (true) 1 else 2)"

  """for {
    |val x <- xs
    |val y <- ys
    |x  > y
    |val z = x + y
    |} yield 2""" ==>
  """for {
    |  val x <- xs
    |  val y <- ys
    |  x > y
    |  val z = x + y
    |} yield 2"""

  """for (
    |    val x <- xs;
    |    val y <- ys;
    |    x  > y;
    |    val z = x + y
    |    ) yield z""" ==>
  """for (
    |  val x <- xs;
    |  val y <- ys;
    |  x > y;
    |  val z = x + y
    |) yield z"""

  """for { 
    |x <- xs
    |y <- ys
    |} 
    |yield
    |2""" ==>
  """for {
    |  x <- xs
    |  y <- ys
    |} yield 2"""

  """for { 
    |x <- xs
    |y <- ys
    |} 
    |println(x + y)""" ==>
  """for {
    |  x <- xs
    |  y <- ys
    |} println(x + y)"""

  "for (n <- 1 to 10 if n > 4 if n < 10) yield n" ==> "for (n <- 1 to 10 if n > 4 if n < 10) yield n"

  "for {n <- 1 to 10 if n > (4)if n < 10} yield n" ==> "for { n <- 1 to 10 if n > (4) if n < 10 } yield n"

  """Some(
    |for (n <- 1 to 10)
    |yield n)""" ==>
  """Some(
    |  for (n <- 1 to 10)
    |    yield n)"""

  """Some(
    |for (n <- 1 to 10)
    |proc())""" ==>
  """Some(
    |  for (n <- 1 to 10)
    |    proc())"""
    
}

 
