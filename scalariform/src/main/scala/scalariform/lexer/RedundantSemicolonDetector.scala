package scalariform.lexer

import scalariform.utils.Range
import scalariform.utils.Utils._

object RedundantSemicolonDetector extends App {

  /**
   * @return all semicolons in the source that could safely be removed without changing the meaning
   *  of the program.
   */
  def findRedundantSemis(source: String) = {

    def isRedundant(semi: Token, index: Int): Boolean = {
      val sourceWithoutSemi = deleteRange(source, semi.range)
      val tokensWithoutSemi = ScalaLexer.tokenise(sourceWithoutSemi)
      val replacementToken = tokensWithoutSemi(index)
      replacementToken.isNewline || replacementToken.tokenType == Tokens.EOF || replacementToken.tokenType == Tokens.RBRACE
    }

    ScalaLexer.tokenise(source).zipWithIndex.collect {
      case (token, index) if token.tokenType == Tokens.SEMI && isRedundant(token, index) ⇒ token
    }

  }

}

object Demo extends App {

  val source = """
    class A { 
      def foo = 42;
      def bar = 123; def baz = 1234 
    };"""
  val redundantSemis = RedundantSemicolonDetector.findRedundantSemis(source)
  val annotated = redundantSemis.reverse.foldLeft(source) { (s, semi) ⇒ replaceRange(s, semi.range, "<;>") }
  println(source)
  println("-------------------------------------------------")
  println(annotated)

}