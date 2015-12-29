package scalariform.astselect

import scalariform.lexer._
import scalariform.parser._
import scalariform.utils.Range
import scala.util.control.Exception._
import scalariform.ScalaVersions

object AstSelector {

  /**
   * Expands the given selection in the source to the range of the closest appropriate
   * enclosing AST element. Returns None if the source does not parse correctly, or if
   * there is no strictly larger containing AST element.
   */
  def expandSelection(source: String, initialSelection: Range, scalaVersion: String = ScalaVersions.DEFAULT_VERSION): Option[Range] =
    catching(classOf[ScalaParserException]).toOption {
      new AstSelector(source, scalaVersion).expandSelection(initialSelection)
    }

  import Tokens._

  private val selectableXmlTokens = Set(XML_NAME, XML_ATTR_VALUE, XML_PCDATA, XML_COMMENT, XML_UNPARSED, XML_PCDATA)

  private val nonSelectableAstNodes: Set[Class[_ <: AstNode]] =
    Set(
      classOf[AccessQualifier],
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
      classOf[TypeParamClause]
    )

}

class AstSelector(source: String, scalaVersion: String = ScalaVersions.DEFAULT_VERSION) {

  import AstSelector._

  private val tokens = ScalaLexer.tokenise(source, scalaVersion = scalaVersion)

  private val compilationUnitOpt: Option[CompilationUnit] = {
    val parser = new ScalaParser(tokens.toArray)
    parser.safeParse(parser.compilationUnitOrScript)
  }

  private val allTokens: List[Token] = tokens.flatMap { token ⇒
    if (token.isNewline)
      token.associatedWhitespaceAndComments.rawTokens
    else
      token.associatedWhitespaceAndComments.rawTokens :+ token
  }

  def expandSelection(initialSelection: Range): Option[Range] =
    expandToToken(initialSelection) orElse
      expandScaladocToAssociatedNode(initialSelection) orElse
      (compilationUnitOpt flatMap { expandToEnclosingAst(_, initialSelection, enclosingNodes = Nil) })

  private def previousToken(token: Token): Option[Token] =
    tokens.indexOf(token) match {
      case 0 | -1 ⇒ None
      case n      ⇒ Some(tokens(n - 1))
    }

  /**
   * If a Scaladoc comment is exactly selected, expand to an associated class, method etc.
   */
  private def expandScaladocToAssociatedNode(initialSelection: Range): Option[Range] =
    for {
      scaladocComment ← allTokens.find { token ⇒ token.isScalaDocComment && token.range == initialSelection }
      associatedNode ← findAssociatedAstNode(scaladocComment)
      nodeRange ← associatedNode.rangeOpt // <-- should always be defined here, in fact
    } yield scaladocComment.range mergeWith nodeRange

  private def isPreviousTokenStringPart(token: Token) =
    previousToken(token).exists(token ⇒ token.tokenType == Tokens.STRING_PART)

  private def expandToStringInterpolationDollarIfPossible(token: Token): Range =
    if (isPreviousTokenStringPart(token))
      token.range.expandLeft(1)
    else
      token.range

  /**
   * If the selection is a strict subrange of some token, expand to the entire token.
   */
  private def expandToToken(initialSelection: Range): Option[Range] =
    allTokens.find { token ⇒
      isSelectableToken(token) && (token.range contains initialSelection) && initialSelection.length < token.length
    }.map(expandToStringInterpolationDollarIfPossible)

  private def findAssociatedAstNode(scaladocCommentToken: Token): Option[AstNode] =
    compilationUnitOpt.flatMap { cu ⇒ findAssociatedAstNode(cu, scaladocCommentToken) }

  private def findAssociatedAstNode(nodeToSearch: AstNode, scaladocCommentToken: Token): Option[AstNode] =
    nodeToSearch.firstTokenOption flatMap { firstToken ⇒
      val hiddenTokens = getPriorHiddenTokens(firstToken)
      if (hiddenTokens.rawTokens.contains(scaladocCommentToken) && !nodeToSearch.isInstanceOf[CompilationUnit])
        Some(nodeToSearch)
      else {
        for {
          childNode ← nodeToSearch.immediateChildren
          result ← findAssociatedAstNode(childNode, scaladocCommentToken)
        } return Some(result)
        None
      }
    }

  private def isSelectableToken(token: Token) = {
    val tokenType = token.tokenType
    import tokenType._
    isLiteral || isKeyword || isComment || isId || tokenType == Tokens.INTERPOLATION_ID ||
      (selectableXmlTokens contains tokenType)
  }

  /**
   * @return range of the node and any Scaladoc immediately before it
   */
  private def adjustedNodeRange(node: AstNode): Option[Range] =
    node.rangeOpt map { nodeRange ⇒
      val scaladocComments = getPriorHiddenTokens(node.firstToken).scalaDocComments
      scaladocComments.lastOption map { comment ⇒ nodeRange mergeWith comment.token.range } getOrElse nodeRange
    }

  /**
   * Attempt to find a suitable AST node to expand to which contains the given selection.
   *
   * @param enclosingNodes -- stack of nodes recording path to root compilation unit (useful for more context-aware
   *   decisions about whether to expand to a node or not).
   */
  private def expandToEnclosingAst(node: AstNode, initialSelection: Range, enclosingNodes: List[AstNode]): Option[Range] = {

    val nodeRange = adjustedNodeRange(node).getOrElse { return None }

    if (!nodeRange.contains(initialSelection)) { return None }

    for {
      childNode ← node.immediateChildren
      descendantRange ← expandToEnclosingAst(childNode, initialSelection, enclosingNodes = node :: enclosingNodes)
    } return Some(descendantRange)

    if (nodeRange.strictlyContains(initialSelection) && isSelectableAst(node :: enclosingNodes))
      if (isPreviousTokenStringPart(node.firstToken)) // Grab $ from string interpolation
        Some(nodeRange.expandLeft(1))
      else
        Some(nodeRange)
    else
      None

  }

  private def getPredecessorNewline(token: Token): Option[HiddenTokens] =
    tokens.indexOf(token) match {
      case 0 ⇒ None
      case n ⇒
        val previousToken = tokens(n - 1)
        if (previousToken.isNewline)
          Some(previousToken.associatedWhitespaceAndComments)
        else
          None
    }

  private def getPriorHiddenTokens(token: Token) = getPredecessorNewline(token) getOrElse token.associatedWhitespaceAndComments

  private def isSelectableAst(nodeStack: List[AstNode]) =
    nodeStack match {
      case List(_: BlockExpr, _: MatchExpr, _*)   ⇒ false
      case List(_: BlockExpr, _: ProcFunBody, _*) ⇒ false
      case List(node, _*)                         ⇒ !(nonSelectableAstNodes contains node.getClass.asInstanceOf[Class[_ <: AstNode]])
      case Nil                                    ⇒ false
    }

}
