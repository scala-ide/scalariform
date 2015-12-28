package scalariform.lexer

import scalariform._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scalariform.utils.Utils._

class RedundantSemicolonDetectorTest extends FlatSpec with ShouldMatchers {

  implicit def stringToCheckable(s: String)(implicit scalaVersion: String = ScalaVersions.DEFAULT_VERSION) =
    new { def check() = checkSemis(s, scalaVersion) }; // Expected redundant semicolons are indicated with <;>

  """
    class A {
      def foo = 42<;>
      def bar = 123; def baz = 1234
    }<;>
  """.check();

  """
    {
      println("Foo")<;>
    }
  """.check();

  """
    class A {
      for (
        x <- 1 to 10;
        y <- 1 to 10
      ) yield x + y<;>
    }
  """.check()

  {
    implicit val scalaVersion = "2.10.0";
    """
      s"my name is ?{person.name<;>}"
    """.replace('?', '$').check
  }

  private def checkSemis(encodedSource: String, scalaVersion: String) {
    val ordinarySource = encodedSource.replace("<;>", ";")
    val semis = RedundantSemicolonDetector.findRedundantSemis(ordinarySource, scalaVersion)
    val encodedSourceAgain = semis.reverse.foldLeft(ordinarySource) { (s, semi) ⇒ replaceRange(s, semi.range, "<;>") }
    encodedSourceAgain should equal(encodedSource)
  }

}
