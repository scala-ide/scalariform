package scalariform.astselect

import org.scalatest._
import org.scalatest.matchers._
import scalariform.utils.Range
import scalariform.ScalaVersions

// format: OFF
class AstSelectorTest extends FlatSpec with ShouldMatchers {

  // Legend:
  //
  // "|" denotes a zero-width selection before the position
  // "$" denotes a single-character-width selection at the position
  // "$$$$$$" denotes a multiple-character-width selection at the given positions

  " wibble  " ~
  "   |     " ~
  " $$$$$$  "

  " wibble  " ~
  " |       " ~
  " $$$$$$  "

  " wibble  " ~
  "      |  " ~
  " $$$$$$  "

  " wibble  " ~
  "       | " ~
  " $$$$$$  "

  " /* foo */ " ~
  "    $      " ~
  " $$$$$$$$$ "

  " /* foo */ /* bar */ " ~
  "     $               " ~
  " $$$$$$$$$           "

  " class A(n: Int) " ~
  "         $$$$$$  " ~
  " $$$$$$$$$$$$$$$ "

  " foo(42) " ~
  "     $$  " ~
  " $$$$$$$ "

  " object A { } " ~
  "           |  " ~
  " $$$$$$$$$$$$ "

  " private def foo = 42 " ~
  "           $$$$$$$$$  " ~
  " $$$$$$$$$$$$$$$$$$$$ "

  " if (a) b else c " ~
  "             $$$ " ~
  " $$$$$$$$$$$$$$$ "

  " aa(bb + cc, dd * ee) " ~
  "              $$$$$   " ~
  "             $$$$$$$  "

  " class A[B] " ~
  "         $  " ~
  " $$$$$$$$$$ "

  " new Wibble " ~
  "     $$$$$$ " ~
  " $$$$$$$$$$ "

  " new Wibble() " ~
  "         $$$  " ~
  " $$$$$$$$$$$$ "

  " a + b + c " ~
  "   $       " ~
  " $$$$$     " ~
  " $$$$$$$$$ "

  " a + b + c " ~
  "      $$$  " ~
  " $$$$$$$$$ "

  " x + y * z " ~
  "      $$$  " ~
  "     $$$$$ " ~
  " $$$$$$$$$ "

  " a :: b :: c :: Nil " ~
  "             $$     " ~
  "           $$$$$$$$ " ~
  "      $$$$$$$$$$$$$ " ~
  " $$$$$$$$$$$$$$$$$$ "

  " a :: b :: Nil ++ Nil " ~
  "                  $$$ " ~
  "           $$$$$$$$$$ " ~
  "      $$$$$$$$$$$$$$$ " ~
  " $$$$$$$$$$$$$$$$$$$$ "

  " a + b :: b + c :: Nil ++ Nil " ~
  "              $               " ~
  "          $$$$$               " ~
  "          $$$$$$$$$$$$$$$$$$$ " ~
  " $$$$$$$$$$$$$$$$$$$$$$$$$$$$ "

  " i += 10 + 2 " ~
  " $           " ~
  " $$$$$$$$$$$ "

  " i += 10 + 2 " ~
  "      $$     " ~
  "      $$$$$$ " ~
  " $$$$$$$$$$$ "

  " 'a'.bar[X](foo).bizzle(a, b).baz.buzz[T].bozz(12)(15).foo _ " ~
  " $$$                                                         " ~
  " $$$$$$$$$$$$$$$                                             " ~
  " $$$$$$$$$$$$$$$$$$$$$$$$$$$$                                " ~
  " $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$                            " ~
  " $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$                    " ~
  " $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$       " ~
  " $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ "

  " a.foo(b).bar(c) " ~
  " $               " ~
  " $$$$$$$$        " ~
  " $$$$$$$$$$$$$$$ "

  " def x = 42 " ~
  "         $$ " ~
  " $$$$$$$$$$ "

  " x: Int " ~
  " $      " ~
  " $$$$$$ "

  " x = a + b " ~
  " $         " ~
  " $$$$$$$$$ "

  " a match { case b => } " ~
  " $                     " ~
  " $$$$$$$$$$$$$$$$$$$$$ "

  " a match { case b => c } " ~
  "           $$$$$$$$$$$   " ~
  " $$$$$$$$$$$$$$$$$$$$$$$ "

  " (a, b) " ~
  "   $$$  " ~
  " $$$$$$ "

  " for { a <- b; c <- d } yield e " ~
  "       $$$$$$$                  " ~
  " $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ "

  " { case b ⇒ c } " ~
  "        $       " ~
  "   $$$$$$$$$$   "

  " for (a ← b) { c } " ~
  "               $   " ~
  "             $$$$$ "

  " for (a <- b if c) {} " ~
  "                $     " ~
  "      $$$$$$$$$$$     "

  " def a { b } " ~
  "         $   " ~
  " $$$$$$$$$$$ "

  " { case b :: c :: d => } " ~
  "                  $      " ~
  "             $$$$$$      " ~
  "        $$$$$$$$$$$      "

  " /** a */ class B " ~
  "                $ " ~
  " $$$$$$$$$$$$$$$$ "

  " /** a */ class B " ~
  "  $$$$            " ~
  " $$$$$$$$         " ~
  " $$$$$$$$$$$$$$$$ "

  " /** a */ class B; class C " ~
  " $$$$$$$$$$$$$$$$          " ~
  " $$$$$$$$$$$$$$$$$$$$$$$$$ "

  " val a  = { b } " ~
  "            $   " ~
  "          $$$$$ " // A bit inconsistent with def's, but maybe OK

  " try a catch b " ~
  "             $ " ~
  " $$$$$$$$$$$$$ "

  " if (a) b " ~
  "     $    " ~
  " $$$$$$$$ "

  " def a = b(c) " ~
  "          $$$ " ~
  "         $$$$ "

  " def a = { b } " ~
  "           $   " ~
  "         $$$$$ "

  " for (a <- b) c(d) " ~
  "               $$$ " ~
  "              $$$$ "

  " def a[B <% C] " ~
  "         $$    " ~
  "       $$$$$$  "

  " class A[B <% C, D <% E] " ~
  "           $$            " ~
  "         $$$$$$          " ~
  " $$$$$$$$$$$$$$$$$$$$$$$ "

  " sealed class A " ~
  "            $$$ " ~
  " $$$$$$$$$$$$$$ "

  " protected[a] val b " ~
  "           $        " ~
  " $$$$$$$$$$$$       "

  " evaluating { stack.pop() } should produce [NoSuchElementException] " ~
  "              $$$$$$$                                               " ~
  "              $$$$$$$$$$$                                           " ~
  "            $$$$$$$$$$$$$$$                                         " ~
  " $$$$$$$$$$$$$$$$$$$$$$$$$$                                         " ~
  " $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ "

  " evaluating { stack.pop() } should produce [NoSuchElementException] " ~
  "                                            $$$$$$$$$$$$$$$$$$$$$$  " ~
  "                                   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ " ~
  " $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ "

  " } wibble  " ~
  "     $     " ~
  "   $$$$$$  "

  " foo.bar.baz.buz() " ~
  "  $                " ~
  " $$$               " ~
  " $$$$$$$           " ~
  " $$$$$$$$$$$       " ~
  " $$$$$$$$$$$$$$$$$ "

  " foo.super[bar].baz " ~
  " $$$                " ~
  " $$$$$$$$$$$$$$     " ~
  " $$$$$$$$$$$$$$$$$$ "

  " package foo.bar.baz " ~
  "         $$$         " ~
  "         $$$$$$$     " ~
  "         $$$$$$$$$$$ " ~
  " $$$$$$$$$$$$$$$$$$$ "

  " foo[bar][baz] " ~
  " $$$           " ~
  " $$$$$$$$      " ~
  " $$$$$$$$$$$$$ "

  " (!foo) " ~
  "   $$$  " ~
  "  $$$$  " ~
  " $$$$$$ "

  /* TODO: Need AST representation for [] calls
  " foo[bar][baz][boz] " ~
  " $$$$$$$$           " ~
  " $$$$$$$$$$$$$      "
  */

  {
    implicit val scalaVersion = "2.10.0"

    """ s"my name is ?{person.name}." """.replace('?', '$') ~
    """                $$$$$$       " """ ~
    """                $$$$$$$$$$$  " """ ~
    """              $$$$$$$$$$$$$$ " """ ~
    """ $$$$$$$$$$$$$$$$$$$$$$$$$$$$$ """

    """ xyz"" """ ~
    """  $    """ ~
    """ $$$   """ ~
    """ $$$$$ """

    """ s"my name is $bob" """ ~
    """    $               """ ~
    """  $$$$$$$$$$$$$     """

    """ s"my name is $bob." """ ~
    """                $    """ ~
    """              $$$$   """

  }

  private def findSelectionRange(s: String): Range = {
    val barLocation = s indexOf '|'
    if (barLocation >= 0)
      Range(barLocation, 0)
    else {
      val firstDollarLocation = s indexOf '$'
      require(firstDollarLocation >= 0, "No selection marker: " + s)
      val dollars = s.drop(firstDollarLocation).takeWhile(_ == '$')
      Range(firstDollarLocation, dollars.length)
    }
  }

  implicit def stringToTestString(source: String)(implicit scalaVersion: String = ScalaVersions.DEFAULT_VERSION): TestString = new TestString(source, scalaVersion)
  class TestString(source: String, scalaVersion: String) {
    def ~(initialSelectionDiagram: String) = IntermediateTest(source, initialSelectionDiagram, scalaVersion)
  }

  case class IntermediateTest(source: String, initialSelectionDiagram: String, scalaVersion: String) {
    def ~(finalSelectionDiagram: String): IntermediateTest = {
       val initialSelection = findSelectionRange(initialSelectionDiagram)
       val actualFinalSelection = AstSelector.expandSelection(source, initialSelection, scalaVersion) getOrElse initialSelection
       val expectedFinalSelection = findSelectionRange(finalSelectionDiagram)
       ("source\n>>>" + source + "<<<\n") should "expand\n>>>" + (initialSelectionDiagram + "<<<\n to \n>>>" + finalSelectionDiagram + "<<<\n") in {
         actualFinalSelection should equal (expectedFinalSelection)
       }
       IntermediateTest(source, initialSelectionDiagram = finalSelectionDiagram, scalaVersion)
    }
  }
}
