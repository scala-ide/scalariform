package scalariform.lexer

import scalariform.utils.Utils._
import scalariform.utils.TextEdit
import scalariform.utils.TextEditProcessor
import scalariform.ScalaVersions

object RedundantSemicolonDetector {

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
