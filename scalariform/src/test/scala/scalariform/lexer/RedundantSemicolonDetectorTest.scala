package scalariform.lexer

import scalariform._
import scalariform.lexer.Tokens._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.TestFailedException
import org.scalatest.TestPendingException
import scalariform.utils.Utils._

class RedundantSemicolonDetectorTest extends FlatSpec with ShouldMatchers {

  implicit def stringToCheckable(s: String) = new { def check = checkSemis(s) } // Expected redundant semicolons are indicated with <;>
;  
  """
    class A { 
      def foo = 42<;>
      def bar = 123; def baz = 1234 
    }<;>
  """.check
;
  """
    { 
      println("Foo")<;>
    }
  """.check
;
  """
    class A { 
      for (
        x <- 1 to 10; 
        y <- 1 to 10
      ) yield x + y<;>
    }
  """.check
;
  private def checkSemis(encodedSource: String) {
    val ordinarySource = encodedSource.replaceAllLiterally("<;>", ";")
    val semis = RedundantSemicolonDetector.findRedundantSemis(ordinarySource)
    val encodedSourceAgain = semis.reverse.foldLeft(ordinarySource) { (s, semi) ⇒ replaceRange(s, semi.range, "<;>") }
    encodedSourceAgain should equal(encodedSource)
  }

}