package scalariform.astselect

import scalariform.lexer._
import scalariform.parser._
import scalariform.utils.Range

object AstSelector {

  /**
   * Expands the given selection in the source to the range of the closest appropriate
   * enclosing AST element. Returns None if the source does not parse correctly, or if
   * there is no strictly larger containing AST element.
   */
  def expandSelection(source: String, initialSelection: Range): Option[Range] =
    try {
      new AstSelector(source).expandSelection(initialSelection)
    } catch {
      case e: ScalaParserException ⇒ None
    }

  import Tokens._

  private val selectableXmls = Set(XML_NAME, XML_ATTR_VALUE, XML_PCDATA, XML_COMMENT, XML_UNPARSED, XML_PCDATA)

  private val nonSelectableAstNodes: Set[Class[_]] =
    Set(
      classOf[CasePattern],
      classOf[ElseClause],
      classOf[Enumerators],
      classOf[ExprFunBody],
      classOf[FunDefOrDcl],
      classOf[ParenArgumentExprs],
      classOf[GeneralTokens],
      classOf[Guard],
      classOf[ParamClause],
      classOf[ParamClauses],
      classOf[PatDefOrDcl],
      classOf[ProcFunBody],
      classOf[Template],
      classOf[TemplateBody],
      classOf[TemplateParents],
      classOf[TypeDefOrDcl],
      classOf[TypeExprElement],
      classOf[TypeParamClause])

}

class AstSelector(source: String) {
  import AstSelector._

  private val (hiddenTokenInfo, tokens) = ScalaLexer.tokeniseFull(source)
  private val compilationUnit = new ScalaParser(tokens.toArray).compilationUnitOrScript()

  def expandSelection(initialSelection: Range): Option[Range] =
    expandToToken(initialSelection) orElse expandToEnclosingAst(compilationUnit, initialSelection, enclosingNodes = Nil)

  private def expandToToken(initialSelection: Range): Option[Range] = {
    val Range(offset, length) = initialSelection
    for (ordinaryToken ← tokens) {
      val allTokens = hiddenTokenInfo.inferredNewlines(ordinaryToken) match {
        case Some(hiddenTokens) ⇒ hiddenTokens.rawTokens
        case None               ⇒ hiddenTokenInfo.hiddenPredecessors(ordinaryToken).rawTokens :+ ordinaryToken
      }
      for {
        token@Token(tokenType, _, startIndex, stopIndex) ← allTokens
        if isSelectableToken(tokenType)
        if offset >= startIndex && offset + length <= stopIndex + 1 && token.length > length
      } return Some(Range(startIndex, token.length))
    }
    None
  }

  private def isSelectableToken(tokenType: TokenType) = {
    import tokenType._
    isLiteral || isKeyword || isComment || isId || (selectableXmls contains tokenType)
  }

  private def expandToEnclosingAst(node: AstNode, initialSelection: Range, enclosingNodes: List[AstNode]): Option[Range] =
    node.rangeOpt flatMap { nodeRange ⇒
      for {
        childNode ← node.immediateChildren
        descendantRange ← expandToEnclosingAst(childNode, initialSelection, enclosingNodes = node :: enclosingNodes)
      } return Some(descendantRange)
      if (nodeRange.contains(initialSelection) && nodeRange.length > initialSelection.length && isSelectableAst(node, enclosingNodes))
        Some(prependScaladocIfPossible(node, nodeRange))
      else
        None
    }

  private def getPredecessorNewline(token: Token): Option[HiddenTokens] =
    tokens.indexOf(token) match {
      case 0 ⇒ None
      case n ⇒ hiddenTokenInfo.inferredNewlines(tokens(n - 1))
    }

  private def prependScaladocIfPossible(node: AstNode, range: Range): Range = {
    val hiddenTokens = getPredecessorNewline(node.firstToken) getOrElse hiddenTokenInfo.hiddenPredecessors(node.firstToken)
    hiddenTokens.scalaDocComments.lastOption match {
      case Some(ScalaDocComment(token)) ⇒
        val commentStart = token.startIndex
        val difference = range.offset - token.startIndex
        Range(token.startIndex, range.length + difference)
      case None ⇒ range
    }
  }

  private def isSelectableAst(node: AstNode, enclosingNodes: List[AstNode]) = {
    // println((node:: enclosingNodes) map (_.getClass.getSimpleName) mkString " ")
    (node :: enclosingNodes) match {
      case n1 :: n2 :: _ if n1.isInstanceOf[BlockExpr] && n2.isInstanceOf[MatchExpr] ⇒ false

      case n1 :: n2 :: n3 :: _ if n1.isInstanceOf[BlockExpr] && n2.isInstanceOf[Expr] && n3.isInstanceOf[ForExpr] ⇒ false
      case n1 :: n2 :: _ if n1.isInstanceOf[Expr] && n2.isInstanceOf[ForExpr] ⇒ false

      case n1 :: n2 :: n3 :: _ if n1.isInstanceOf[BlockExpr] && n2.isInstanceOf[Expr] && n3.isInstanceOf[ExprFunBody] ⇒ false
      case n1 :: n2 :: _ if n1.isInstanceOf[Expr] && n2.isInstanceOf[ExprFunBody] ⇒ false

      case n1 :: n2 :: _ if n1.isInstanceOf[BlockExpr] && n2.isInstanceOf[ProcFunBody] ⇒ false

      case _ ⇒ !(nonSelectableAstNodes contains node.getClass)
    }
  }
}
