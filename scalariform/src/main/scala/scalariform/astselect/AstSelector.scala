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
      classOf[CatchClause],
      classOf[CondExpr],
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
      classOf[TmplDef],
      classOf[TypeDefOrDcl],
      classOf[TypeExprElement],
      classOf[TypeParamClause])

}

class AstSelector(source: String) {
  import AstSelector._

  private val (hiddenTokenInfo, tokens) = ScalaLexer.tokeniseFull(source)
  import hiddenTokenInfo._

  private val compilationUnit = new ScalaParser(tokens.toArray).compilationUnitOrScript()

  /**
   * A node's adjusted range includes any Scaladoc immediately before it.
   */
  private def adjustedNodeRange(node: AstNode): Option[Range] =
    if (node.isEmpty)
      None
    else {
      val nodeRange = node.rangeOpt.get
      val scaladocComments = getPriorHiddenTokens(node.firstToken).scalaDocComments
      scaladocComments.lastOption map { comment ⇒ nodeRange mergeWith comment.token.range } orElse Some(nodeRange)
    }

  def expandSelection(initialSelection: Range): Option[Range] =
    expandToToken(initialSelection) orElse expandToEnclosingAst(compilationUnit, initialSelection, enclosingNodes = Nil)

  private def expandToToken(initialSelection: Range): Option[Range] = {
    val Range(offset, length) = initialSelection
    for (ordinaryToken ← tokens) {
      val allTokens = inferredNewlines(ordinaryToken) match {
        case Some(hiddenTokens) ⇒ hiddenTokens.rawTokens
        case None               ⇒ hiddenPredecessors(ordinaryToken).rawTokens :+ ordinaryToken
      }
      for {
        token ← allTokens
        if isSelectableToken(token.tokenType)
        if token.range contains initialSelection
      } {
        if (token.isScalaDocComment && token.range == initialSelection)
          for (expandedToAstRange ← appendAstNodeIfPossible(compilationUnit, token))
            return Some(expandedToAstRange)
        if (token.length > length)
          return Some(token.range)
      }
    }
    None
  }

  private def appendAstNodeIfPossible(node: AstNode, commentToken: Token): Option[Range] =
    node.firstTokenOption flatMap { firstToken ⇒
      val hiddenTokens = getPriorHiddenTokens(firstToken)
      if (hiddenTokens.rawTokens contains commentToken)
        Some(commentToken.range mergeWith node.rangeOpt.get)
      else {
        for {
          childNode ← node.immediateChildren
          result ← appendAstNodeIfPossible(childNode, commentToken)
        } return Some(result)
        None
      }
    }

  private def isSelectableToken(tokenType: TokenType) = {
    import tokenType._
    isLiteral || isKeyword || isComment || isId || (selectableXmls contains tokenType)
  }

  private def expandToEnclosingAst(node: AstNode, initialSelection: Range, enclosingNodes: List[AstNode]): Option[Range] =
    adjustedNodeRange(node) flatMap { nodeRange ⇒
      if (nodeRange contains initialSelection) {
        for {
          childNode ← node.immediateChildren
          descendantRange ← expandToEnclosingAst(childNode, initialSelection, enclosingNodes = node :: enclosingNodes)
        } return Some(descendantRange)
        if ((nodeRange contains initialSelection) && (nodeRange isLargerThan initialSelection) && isSelectableAst(node, enclosingNodes))
          Some(nodeRange)
        else
          None
      } else
        None
    }

  private def getPredecessorNewline(token: Token): Option[HiddenTokens] =
    tokens.indexOf(token) match {
      case 0 ⇒ None
      case n ⇒ inferredNewlines(tokens(n - 1))
    }

  private def getPriorHiddenTokens(token: Token) = getPredecessorNewline(token) getOrElse hiddenPredecessors(token)

  private def isSelectableAst(node: AstNode, enclosingNodes: List[AstNode]) = {
    // println((node:: enclosingNodes) map (_.getClass.getSimpleName) mkString " ")
    (node :: enclosingNodes) match {
      case n1 :: n2 :: _ if n1.isInstanceOf[BlockExpr] && n2.isInstanceOf[MatchExpr] ⇒ false

      // case n1 :: n2 :: n3 :: _ if n1.isInstanceOf[BlockExpr] && n2.isInstanceOf[Expr] && n3.isInstanceOf[ForExpr] ⇒ false
      // case n1 :: n2 :: _ if n1.isInstanceOf[Expr] && n2.isInstanceOf[ForExpr] ⇒ false

      // case n1 :: n2 :: n3 :: _ if n1.isInstanceOf[BlockExpr] && n2.isInstanceOf[Expr] && n3.isInstanceOf[ExprFunBody] ⇒ false
      // case n1 :: n2 :: _ if n1.isInstanceOf[Expr] && n2.isInstanceOf[ExprFunBody] ⇒ false

      case n1 :: n2 :: _ if n1.isInstanceOf[BlockExpr] && n2.isInstanceOf[ProcFunBody] ⇒ false

      case _ ⇒ !(nonSelectableAstNodes contains node.getClass)
    }
  }
}
