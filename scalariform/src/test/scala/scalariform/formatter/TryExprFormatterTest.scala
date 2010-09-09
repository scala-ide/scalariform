package scalariform.formatter

import scalariform.parser._
import scalariform.formatter._

// format: OFF
class TryExprFormatterTest extends AbstractExpressionFormatterTest {

  override val debug = false

  "try  println()" ==> "try println()"

  """try 
    |println()""" ==>
  """try
    |  println()"""

  """try 
    | {
    |println()
    | }""" ==>
  """try {
    |  println()
    |}"""

  """try{println()
    | }""" ==>
  """try {
    |  println()
    |}"""

  "try{println()}" ==> "try { println() }"

  """try {
    |println()
    |}
    |catch { case e => }""" ==>
  """try {
    |  println()
    |} catch { case e => }"""

  "try { foo() }catch{ case e => }" ==> "try { foo() } catch { case e => }"

  """try { foo() } 
    |catch{ case e => }""" ==>
  """try { foo() }
    |catch { case e => }"""

 """try { 
    |foo() } 
    |catch{ case e => }""" ==>
  """try {
    |  foo()
    |} catch { case e => }"""

  """try {
    |} catch { case e => }finally 
    |{ 
    |println("foo") }""" ==>
  """try {
    |} catch { case e => }
    |finally {
    |  println("foo")
    |}"""

  "try {} catch { case e => } finally {}" ==> "try {} catch { case e => } finally {}"

  """try {} 
    |catch { case e => } 
    |finally {}""" ==>
  """try {}
    |catch { case e => }
    |finally {}"""

  """try {} catch { case e => } 
    |finally{
    |resource.close()
    |}""" ==>
  """try {} catch { case e => }
    |finally {
    |  resource.close()
    |}"""

  """try {} catch { case e => } 
    |finally resource.close()""" ==>
  """try {} catch { case e => }
    |finally resource.close()"""

  """try 
    |resource.useIt()
    |catch { case e => } 
    |finally 
    |resource.close()""" ==>
  """try
    |  resource.useIt()
    |catch { case e => }
    |finally
    |  resource.close()"""

  """try {
    |  foo()
    |} catch {
    |  case _ => bar()
    |}""" ==>
  """try {
    |  foo()
    |} catch {
    |  case _ => bar()
    |}"""

  """try {
    |println("bar")
    |} finally {
    |println("foo")
    |}""" ==>
  """try {
    |  println("bar")
    |} finally {
    |  println("foo")
    |}"""


}
