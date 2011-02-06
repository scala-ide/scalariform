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
      classOf[ElseClause],
      classOf[ExprFunBody],
      classOf[FunDefOrDcl],
      classOf[ParenArgumentExprs],
      classOf[ParamClause],
      classOf[ParamClauses],
      classOf[PatDefOrDcl],
      classOf[Template],
      classOf[TemplateBody],
      classOf[TemplateParents],
      classOf[TypeDefOrDcl],
      classOf[TypeExprElement],
      classOf[TypeParamClause])

  private val nonSelectableChildParentNodes: Set[(Class[_], Class[_])] =
    Set(
      (classOf[BlockExpr], classOf[MatchExpr]))
}

class AstSelector(source: String) {
  import AstSelector._

  private val (hiddenTokenInfo, tokens) = ScalaLexer.tokeniseFull(source)
  private val compilationUnit = new ScalaParser(tokens.toArray).compilationUnitOrScript()

  def expandSelection(initialSelection: Range): Option[Range] =
    expandToToken(initialSelection) orElse expandToEnclosingAst(compilationUnit, initialSelection, parent = None)

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

  private def expandToEnclosingAst(node: AstNode, initialSelection: Range, parent: Option[AstNode]): Option[Range] =
    node.rangeOpt flatMap { nodeRange ⇒
      for {
        childNode ← node.immediateChildren
        descendantRange ← expandToEnclosingAst(childNode, initialSelection, parent = Some(node))
      } return Some(descendantRange)
      if (nodeRange.contains(initialSelection) && nodeRange.length > initialSelection.length && isSelectableAst(node, parent))
        Some(nodeRange)
      else
        None
    }

  private def isSelectableAst(node: AstNode, parentOpt: Option[AstNode]) =
    if (nonSelectableAstNodes contains node.getClass)
      false
    else
      !(parentOpt exists { parent ⇒ nonSelectableChildParentNodes contains (node.getClass, parent.getClass) })

}
