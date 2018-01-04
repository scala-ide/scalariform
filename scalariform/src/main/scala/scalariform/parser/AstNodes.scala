package scalariform.parser

import scalariform.lexer.Token
import scalariform.utils.Range

sealed trait AstNode extends Product {

  def tokens: List[Token]

  def firstTokenOption: Option[Token] = tokens.headOption

  lazy val lastTokenOption: Option[Token] = tokens.lastOption

  def firstToken: Token = firstTokenOption.get

  lazy val lastToken: Token = lastTokenOption.get

  protected trait Flattenable {
    def tokens: List[Token]
  }

  def isEmpty: Boolean = tokens.isEmpty

  protected implicit def astNodeToFlattenable(node: AstNode): Flattenable = new Flattenable { val tokens: List[Token] = node.tokens }
  protected implicit def listToFlattenable[T](list: List[T])(implicit flat: T => Flattenable): Flattenable = new Flattenable { val tokens: List[Token] = list flatMap { _.tokens } }
  protected implicit def optionToFlattenable[T](option: Option[T])(implicit flat: T => Flattenable): Flattenable = new Flattenable { val tokens: List[Token] = option.toList flatMap { _.tokens } }
  protected implicit def pairToFlattenable[T1, T2](pair: (T1, T2))(implicit flat1: T1 => Flattenable, flat2: T2 => Flattenable): Flattenable = new Flattenable { val tokens: List[Token] = pair._1.tokens ::: pair._2.tokens }
  protected implicit def tripleToFlattenable[T1, T2, T3](triple: (T1, T2, T3))(implicit flat1: T1 => Flattenable, flat2: T2 => Flattenable, flat3: T3 => Flattenable): Flattenable = new Flattenable { val tokens: List[Token] = triple._1.tokens ++ triple._2.tokens ++ triple._3.tokens }
  protected implicit def eitherToFlattenable[T1, T2](either: T1 Either T2)(implicit flat1: T1 => Flattenable, flat2: T2 => Flattenable): Flattenable = new Flattenable {
    val tokens: List[Token] = either match {
      case Left(f)  ⇒ f.tokens
      case Right(f) ⇒ f.tokens
    }
  }
  protected implicit def tokenToFlattenable(token: Token): Flattenable = new Flattenable { val tokens = List(token) }

  protected def flatten(flattenables: Flattenable*): List[Token] = flattenables.toList flatMap { _.tokens }

  def immediateChildren: List[AstNode] = productIterator.toList flatten immediateAstNodes

  private def immediateAstNodes(n: Any): List[AstNode] = n match {
    case a: AstNode                ⇒ List(a)
    case _: Token                  ⇒ Nil
    case Some(x)                   ⇒ immediateAstNodes(x)
    case xs @ (_ :: _)             ⇒ xs flatMap { immediateAstNodes _ }
    case Left(x)                   ⇒ immediateAstNodes(x)
    case Right(x)                  ⇒ immediateAstNodes(x)
    case (l, r)                    ⇒ immediateAstNodes(l) ++ immediateAstNodes(r)
    case (x, y, z)                 ⇒ immediateAstNodes(x) ++ immediateAstNodes(y) ++ immediateAstNodes(z)
    case true | false | Nil | None ⇒ Nil
  }

  /**
   * Returns range of tokens in the node, or None if there are no tokens in the node
   */
  def rangeOpt: Option[Range] =
    if (tokens.isEmpty)
      None
    else {
      val firstIndex = tokens.head.offset
      val lastIndex = tokens.last.lastCharacterOffset
      Some(Range(firstIndex, lastIndex - firstIndex + 1))
    }

}

case class GeneralTokens(toks: List[Token]) extends AstNode with TypeElement with ExprElement {
  lazy val tokens: List[Token] = flatten(toks)
}

case class Refinement(lbrace: Token, refineStatSeq: StatSeq, rbrace: Token) extends AstNode with TypeElement {
  lazy val tokens: List[Token] = flatten(lbrace, refineStatSeq, rbrace)
}

case class TypeParam(contents: List[TypeElement]) extends AstNode with TypeElement {
  lazy val tokens: List[Token] = flatten(contents)
}

case class TypeParamClause(contents: List[TypeElement]) extends AstNode with TypeElement {
  //require(!contents.isEmpty)
  lazy val tokens: List[Token] = flatten(contents)
}

case class Annotation(at: Token, annotationType: Type, argumentExprss: List[ArgumentExprs], newlineOption: Option[Token]) extends TypeElement with ExprElement {
  lazy val tokens: List[Token] = flatten(at, annotationType, argumentExprss, newlineOption)
}

case class InfixTypeConstructor(id: Token) extends AstNode with TypeElement {
  lazy val tokens: List[Token] = flatten(id)
}

sealed trait TypeElement extends AstNode

case class Type(contents: List[TypeElement]) extends AstNode with TypeElement {
  //require(!contents.isEmpty)
  lazy val tokens: List[Token] = flatten(contents)
}

case class VarianceTypeElement(id: Token) extends AstNode with TypeElement {
  lazy val tokens: List[Token] = flatten(id)
}

case class VarargsTypeElement(star: Token) extends AstNode with TypeElement {
  lazy val tokens: List[Token] = flatten(star)
}
case class CallByNameTypeElement(arrow: Token) extends AstNode with TypeElement {
  lazy val tokens: List[Token] = flatten(arrow)
}

sealed trait ExprElement extends AstNode

case class Expr(contents: List[ExprElement]) extends AstNode with ExprElement with Stat with Enumerator with XmlContents with ImportExpr {
  lazy val tokens: List[Token] = flatten(contents)
}

case class ParenExpr(lparen: Token, contents: List[ExprElement], rparen: Token) extends ExprElement {
  lazy val tokens: List[Token] = flatten(lparen, contents, rparen)
}

case class PrefixExprElement(id: Token) extends ExprElement {
  lazy val tokens: List[Token] = flatten(id)
}

case class PostfixExpr(first: List[ExprElement], postfixId: Token) extends ExprElement {
  lazy val tokens: List[Token] = flatten(first, postfixId)
}

case class InfixExpr(left: List[ExprElement], infixId: Token, newlineOption: Option[Token], right: List[ExprElement]) extends ExprElement {
  lazy val tokens: List[Token] = flatten(left, infixId, newlineOption, right)
}

case class CallExpr(
  exprDotOpt:                   Option[(List[ExprElement], Token)],
  id:                           Token,
  typeArgsOpt:                  Option[TypeExprElement]              = None,
  newLineOptsAndArgumentExprss: List[(Option[Token], ArgumentExprs)] = Nil,
  uscoreOpt:                    Option[Token]                        = None
) extends ExprElement {
  lazy val tokens: List[Token] = flatten(exprDotOpt, id, typeArgsOpt, newLineOptsAndArgumentExprss, uscoreOpt)
}

case class TypeExprElement(contents: List[TypeElement]) extends AstNode with ExprElement {
  //require(!contents.isEmpty)
  lazy val tokens: List[Token] = flatten(contents)
}

trait ArgumentExprs extends ExprElement

case class BlockArgumentExprs(contents: List[ExprElement]) extends ArgumentExprs {
  lazy val tokens: List[Token] = flatten(contents)
}

case class ParenArgumentExprs(lparen: Token, contents: List[ExprElement], rparen: Token) extends ArgumentExprs {
  lazy val tokens: List[Token] = flatten(lparen, contents, rparen)
}

case class Argument(expr: Expr) extends AstNode with ExprElement {
  lazy val tokens: List[Token] = flatten(expr)
}

case class New(newToken: Token, template: Template) extends ExprElement {
  lazy val tokens: List[Token] = flatten(newToken, template)
}

case class IfExpr(
  ifToken:     Token,
  condExpr:    CondExpr,
  newlinesOpt: Option[Token],
  body:        Expr,
  elseClause:  Option[ElseClause]
) extends AstNode with ExprElement {

  lazy val tokens: List[Token] = flatten(ifToken, condExpr, newlinesOpt, body, elseClause)

}

case class ElseClause(semiOpt: Option[Token], elseToken: Token, elseBody: Expr) extends AstNode {
  lazy val tokens: List[Token] = flatten(semiOpt, elseToken, elseBody)
}

case class BlockExpr(lbrace: Token, caseClausesOrStatSeq: Either[CaseClauses, StatSeq], rbrace: Token) extends AstNode with ExprElement {
  lazy val tokens: List[Token] = flatten(lbrace, caseClausesOrStatSeq, rbrace)
}

case class CondExpr(lparen: Token, condition: Expr, rparen: Token) extends AstNode {
  lazy val tokens: List[Token] = flatten(lparen, condition, rparen)
}

case class WhileExpr(whileToken: Token, condExpr: CondExpr, newlinesOpt: Option[Token], body: Expr) extends AstNode with ExprElement {
  lazy val tokens: List[Token] = flatten(whileToken, condExpr, newlinesOpt, body)
}

case class DoExpr(doToken: Token, body: Expr, statSepOpt: Option[Token], whileToken: Token, condExpr: CondExpr) extends AstNode with ExprElement {
  lazy val tokens: List[Token] = flatten(doToken, body, statSepOpt, whileToken, condExpr)
}

case class ForExpr(
  forToken:       Token,
  lParenOrBrace:  Token,
  enumerators:    Enumerators,
  rParenOrBrace:  Token,
  newlinesOption: Option[Token],
  yieldOption:    Option[Token],
  body:           Expr
) extends AstNode with ExprElement {

  lazy val tokens: List[Token] = flatten(forToken, lParenOrBrace, enumerators, rParenOrBrace, newlinesOption, yieldOption, body)

}

sealed trait Enumerator extends AstNode

case class Enumerators(initialGenerator: Generator, rest: List[(Token, Enumerator)]) extends AstNode {
  lazy val tokens: List[Token] = flatten(initialGenerator, rest)
}

case class Generator(
  valOption:          Option[Token],
  pattern:            Expr,
  equalsOrArrowToken: Token,
  expr:               Expr,
  guards:             List[Guard]
) extends AstNode with Enumerator {

  lazy val tokens: List[Token] = flatten(valOption, pattern, equalsOrArrowToken, expr, guards)

}

case class Guard(ifToken: Token, expr: Expr) extends AstNode with Enumerator {
  lazy val tokens: List[Token] = flatten(ifToken, expr)
}

case class CatchClause(catchToken: Token, catchBlockOrExpr: Either[BlockExpr, Expr]) extends AstNode {
  lazy val tokens: List[Token] = flatten(catchToken, catchBlockOrExpr)
}

case class TryExpr(tryToken: Token, body: Expr, catchClauseOption: Option[CatchClause], finallyClauseOption: Option[(Token, Expr)]) extends AstNode with ExprElement {
  lazy val tokens: List[Token] = flatten(tryToken, body, catchClauseOption, finallyClauseOption)
}

case class FullDefOrDcl(annotations: List[Annotation], modifiers: List[Modifier], defOrDcl: DefOrDcl) extends Stat {
  lazy val tokens: List[Token] = flatten(annotations, modifiers, defOrDcl)
}

case class MatchExpr(left: List[ExprElement], matchToken: Token, block: BlockExpr) extends ExprElement {
  lazy val tokens: List[Token] = flatten(left, matchToken, block)
}

case class AscriptionExpr(left: List[ExprElement], colon: Token, right: List[ExprElement]) extends ExprElement {
  lazy val tokens: List[Token] = flatten(left, colon, right)
}

case class EqualsExpr(lhs: List[ExprElement], equals: Token, rhs: Expr) extends ExprElement {
  lazy val tokens: List[Token] = flatten(lhs, equals, rhs)
}

case class CaseClause(casePattern: CasePattern, statSeq: StatSeq) extends AstNode {
  lazy val tokens: List[Token] = flatten(casePattern, statSeq)
}

case class CasePattern(caseToken: Token, pattern: Expr, guardOption: Option[Guard], arrow: Token) extends AstNode {
  lazy val tokens: List[Token] = flatten(caseToken, pattern, guardOption, arrow)
}

case class CaseClauses(caseClauses: List[CaseClause]) extends AstNode {
  //require(!caseClauses.isEmpty)
  lazy val tokens: List[Token] = flatten(caseClauses)
}

sealed trait DefOrDcl extends AstNode

case class TypeDefOrDcl(contents: List[TypeElement]) extends DefOrDcl {
  //require(!contents.isEmpty)
  lazy val tokens: List[Token] = flatten(contents)
}

case class PatDefOrDcl(
  valOrVarToken:      Token,
  pattern:            Expr,
  otherPatterns:      List[(Token, Expr)],
  typedOpt:           Option[(Token, Type)],
  equalsClauseOption: Option[(Token, Expr)]
) extends DefOrDcl {

  lazy val tokens: List[Token] = flatten(valOrVarToken, pattern, otherPatterns, typedOpt, equalsClauseOption)

}

sealed trait FunBody extends AstNode

case class ProcFunBody(newlineOpt: Option[Token], bodyBlock: BlockExpr) extends FunBody {
  lazy val tokens: List[Token] = flatten(newlineOpt, bodyBlock)
}

case class ExprFunBody(equals: Token, macroOpt: Option[Token], body: Expr) extends FunBody {
  lazy val tokens: List[Token] = flatten(equals, macroOpt, body)
}

case class ParamClauses(newlineOpt: Option[Token], paramClausesAndNewlines: List[(ParamClause, Option[Token])]) extends AstNode {
  lazy val tokens: List[Token] = flatten(newlineOpt, paramClausesAndNewlines)
}

case class ParamClause(lparen: Token, implicitOption: Option[Token], firstParamOption: Option[Param], otherParams: List[(Token, Param)], rparen: Token) extends AstNode {
  lazy val tokens: List[Token] = flatten(lparen, implicitOption, firstParamOption, otherParams, rparen)
}

case class Param(annotations: List[Annotation], modifiers: List[Modifier], valOrVarOpt: Option[Token], id: Token, paramTypeOpt: Option[(Token, Type)], defaultValueOpt: Option[(Token, Expr)]) extends AstNode {
  lazy val tokens: List[Token] = flatten(annotations, modifiers, valOrVarOpt, id, paramTypeOpt, defaultValueOpt)
}

case class FunDefOrDcl(
  defToken:           Token,
  nameToken:          Token, // id or THIS
  typeParamClauseOpt: Option[TypeParamClause],
  paramClauses:       ParamClauses,
  returnTypeOpt:      Option[(Token, Type)],
  funBodyOpt:         Option[FunBody],
  localDef:           Boolean
) extends DefOrDcl {

  lazy val tokens: List[Token] = flatten(defToken, nameToken, typeParamClauseOpt, paramClauses, returnTypeOpt, funBodyOpt)

}

case class TmplDef(
  markerTokens:                  List[Token],
  name:                          Token,
  typeParamClauseOpt:            Option[TypeParamClause],
  annotations:                   List[Annotation],
  accessModifierOpt:             Option[AccessModifier],
  paramClausesOpt:               Option[ParamClauses],
  templateInheritanceSectionOpt: Option[TemplateInheritanceSection],
  templateBodyOption:            Option[TemplateBody]
) extends DefOrDcl {
  //require(markerTokens.size <= 2)
  lazy val tokens: List[Token] = flatten(markerTokens, name, typeParamClauseOpt, annotations, accessModifierOpt, paramClausesOpt, templateInheritanceSectionOpt, templateBodyOption)

}

case class TemplateInheritanceSection(
  extendsOrSubtype:   Token,
  earlyDefsOpt:       Option[EarlyDefs],
  templateParentsOpt: Option[TemplateParents]
) extends AstNode {

  lazy val tokens: List[Token] = flatten(extendsOrSubtype, earlyDefsOpt, templateParentsOpt)

}

case class EarlyDefs(earlyBody: TemplateBody, withOpt: Option[Token]) extends AstNode {
  lazy val tokens: List[Token] = flatten(earlyBody, withOpt)
}

case class Template(earlyDefsOpt: Option[EarlyDefs], templateParentsOpt: Option[TemplateParents], templateBodyOpt: Option[TemplateBody]) extends ExprElement {
  lazy val tokens: List[Token] = flatten(earlyDefsOpt, templateParentsOpt, templateBodyOpt)
}

case class TemplateBody(newlineOpt: Option[Token], lbrace: Token, statSeq: StatSeq, rbrace: Token) extends AstNode {
  lazy val tokens: List[Token] = flatten(newlineOpt, lbrace, statSeq, rbrace)
}

sealed trait Stat extends AstNode

case class StatSeq(
  selfReferenceOpt: Option[(Expr, Token)],
  firstStatOpt:     Option[Stat],
  otherStats:       List[(Token, Option[Stat])]
) extends AstNode with ExprElement {

  lazy val tokens: List[Token] = flatten(selfReferenceOpt, firstStatOpt, otherStats)

}

case class TemplateParents(typeAndArgs: (Type, List[ArgumentExprs]), withTypesAndArgs: List[(Token, Type, List[ArgumentExprs])]) extends AstNode {
  lazy val tokens: List[Token] = flatten(typeAndArgs, withTypesAndArgs)
}

case class ImportClause(importToken: Token, importExpr: ImportExpr, otherImportExprs: List[(Token, ImportExpr)]) extends AstNode with Stat {
  lazy val tokens: List[Token] = flatten(importToken, importExpr, otherImportExprs)
}

sealed trait ImportExpr extends AstNode

case class BlockImportExpr(prefixExpr: Expr, importSelectors: ImportSelectors) extends ImportExpr {
  lazy val tokens: List[Token] = flatten(prefixExpr, importSelectors)
}

case class ImportSelectors(lbrace: Token, firstImportSelector: Expr, otherImportSelectors: List[(Token, Expr)], rbrace: Token) extends AstNode {
  lazy val tokens: List[Token] = flatten(lbrace, firstImportSelector, otherImportSelectors, rbrace)
}

case class PackageBlock(packageToken: Token, name: CallExpr, newlineOpt: Option[Token], lbrace: Token, topStats: StatSeq, rbrace: Token) extends Stat {
  lazy val tokens: List[Token] = flatten(packageToken, name, newlineOpt, lbrace, topStats, rbrace)
}

case class PackageStat(packageToken: Token, name: CallExpr) extends Stat {
  lazy val tokens: List[Token] = flatten(packageToken, name)
}

sealed trait Modifier extends AstNode

case class SimpleModifier(token: Token) extends Modifier {
  lazy val tokens: List[Token] = flatten(token)
}

case class AccessModifier(privateOrProtected: Token, accessQualifierOpt: Option[AccessQualifier]) extends Modifier {
  lazy val tokens: List[Token] = flatten(privateOrProtected, accessQualifierOpt)
}
case class AccessQualifier(lbracket: Token, thisOrId: Token, rbracket: Token) extends AstNode {
  lazy val tokens: List[Token] = flatten(lbracket, thisOrId, rbracket)
}

case class CompilationUnit(topStats: StatSeq, eofToken: Token) extends AstNode {
  lazy val tokens: List[Token] = flatten(topStats, eofToken)
}

case class AnonymousFunctionStart(parameters: List[ExprElement], arrow: Token) extends ExprElement {
  lazy val tokens: List[Token] = flatten(parameters, arrow)
}

case class AnonymousFunction(parameters: List[ExprElement], arrow: Token, body: StatSeq) extends ExprElement {
  lazy val tokens: List[Token] = flatten(parameters, arrow, body)
}

case class StringInterpolation(interpolationId: Token, stringPartsAndScala: List[(Token, Expr)], terminalString: Token) extends ExprElement {
  lazy val tokens: List[Token] = flatten(interpolationId, stringPartsAndScala, terminalString)
}

sealed trait XmlExprElement extends ExprElement

case class XmlStartTag(startOpen: Token, name: Token, attributes: List[(Option[Token], XmlAttribute)], whitespaceOption: Option[Token], tagClose: Token) extends XmlExprElement {
  lazy val tokens: List[Token] = flatten(startOpen, name, attributes, whitespaceOption, tagClose)
}

case class XmlAttribute(name: Token, whitespaceOption: Option[Token], equals: Token, whitespaceOption2: Option[Token], valueOrEmbeddedScala: Either[Token, Expr]) extends XmlExprElement {
  lazy val tokens: List[Token] = flatten(name, whitespaceOption, equals, whitespaceOption2, valueOrEmbeddedScala)
}

case class XmlEmptyElement(startOpen: Token, name: Token, attributes: List[(Option[Token], XmlAttribute)], whitespaceOption: Option[Token], emptyClose: Token) extends XmlElement {
  lazy val tokens: List[Token] = flatten(startOpen, name, attributes, whitespaceOption, emptyClose)
}

case class XmlEndTag(endOpen: Token, name: Token, whitespaceOption: Option[Token], tagClose: Token) extends XmlExprElement {
  lazy val tokens: List[Token] = flatten(endOpen, name, whitespaceOption, tagClose)
}

sealed trait XmlElement extends XmlContents

case class XmlNonEmptyElement(startTag: XmlStartTag, contents: List[XmlContents], endTag: XmlEndTag) extends XmlElement {
  lazy val tokens: List[Token] = flatten(startTag, contents, endTag)
}

sealed trait XmlContents extends XmlExprElement

case class XmlPCDATA(token: Token) extends XmlContents { lazy val tokens: List[Token] = flatten(token) }
case class XmlCDATA(token: Token) extends XmlContents { lazy val tokens: List[Token] = flatten(token) }
case class XmlComment(token: Token) extends XmlContents { lazy val tokens: List[Token] = flatten(token) }
case class XmlUnparsed(token: Token) extends XmlContents { lazy val tokens: List[Token] = flatten(token) }
case class XmlProcessingInstruction(token: Token) extends XmlContents { lazy val tokens: List[Token] = flatten(token) }

case class XmlExpr(first: XmlContents, otherElements: List[XmlContents]) extends ExprElement {
  lazy val tokens: List[Token] = flatten(first, otherElements)
}
