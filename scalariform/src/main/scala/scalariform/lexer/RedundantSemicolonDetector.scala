package scalariform.lexer

import scalariform.utils.Range
import scalariform.utils.Utils._
import scalariform.utils.TextEdit
import scalariform.utils.TextEditProcessor
import scalariform.ScalaVersions

object RedundantSemicolonDetector extends App {

  /**
   * @return all semicolons in the source that could safely be removed without changing the meaning
   *  of the program.
   */
  def findRedundantSemis(source: String, scalaVersion: String = ScalaVersions.DEFAULT_VERSION): List[Token] = {

    def isRedundant(semi: Token, index: Int): Boolean = {
      val sourceWithoutSemi = deleteRange(source, semi.range)
      val tokensWithoutSemi = ScalaLexer.tokenise(sourceWithoutSemi, forgiveErrors = true, scalaVersion = scalaVersion)
      val replacementToken = tokensWithoutSemi(index)
      replacementToken.isNewline || replacementToken.tokenType == Tokens.EOF || replacementToken.tokenType == Tokens.RBRACE
    }

    ScalaLexer.tokenise(source, forgiveErrors = true, scalaVersion = scalaVersion).zipWithIndex.collect {
      case (token, index) if token.tokenType == Tokens.SEMI && isRedundant(token, index) ⇒ token
    }

  }

  def removeRedundantSemis(s: String): String =
    TextEditProcessor.runEdits(s, getEditsToRemoveRedundantSemis(s))

  def getEditsToRemoveRedundantSemis(s: String): List[TextEdit] =
    findRedundantSemis(s).map(_.range).map(TextEdit.delete)

}

object Demo extends App {

  val source = """
    class A {
      def foo = 42;
      def bar = 123; def baz = 1234
    };"""
  val redundantSemis = RedundantSemicolonDetector.findRedundantSemis(source)
  val annotated = redundantSemis.reverse.foldLeft(source) { (s, semi) ⇒ replaceRange(s, semi.range, "<;>") }
  val purged = RedundantSemicolonDetector.removeRedundantSemis(source)
  println(source)
  println("-------------------------------------------------")
  println(annotated)
  println("-------------------------------------------------")
  println(purged)
}