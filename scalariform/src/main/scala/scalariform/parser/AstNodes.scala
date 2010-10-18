package scalariform.parser

import scalariform.utils.CaseClassReflector

import scalariform.lexer.Token

trait AstNode extends CaseClassReflector {

  def tokens: List[Token]

  def firstTokenOption: Option[Token] = tokens.headOption

  lazy val lastTokenOption: Option[Token] = tokens.lastOption

  def firstToken = firstTokenOption.get

  lazy val lastToken = lastTokenOption.get

  protected trait Flattenable {
    def tokens: List[Token]
  }

  protected implicit def astNodeToFlattenable(node: AstNode): Flattenable = new Flattenable { val tokens = node.tokens }
  protected implicit def listToFlattenable[T <% Flattenable](list: List[T]): Flattenable = new Flattenable { val tokens = list flatMap { _.tokens } }
  protected implicit def optionToFlattenable[T <% Flattenable](option: Option[T]): Flattenable = new Flattenable { val tokens = option.toList flatMap { _.tokens } }
  protected implicit def pairToFlattenable[T1 <% Flattenable, T2 <% Flattenable](pair: (T1, T2)): Flattenable = new Flattenable { val tokens = pair._1.tokens ::: pair._2.tokens }
  protected implicit def eitherToFlattenable[T1 <% Flattenable, T2 <% Flattenable](either: T1 Either T2): Flattenable = new Flattenable {
    val tokens = either match {
      case Left(f) ⇒ f.tokens
      case Right(f) ⇒ f.tokens
    }
  }
  protected implicit def tokenToFlattenable(token: Token): Flattenable = new Flattenable { val tokens = List(token) }

  protected def flatten(flattenables: Flattenable*): List[Token] = flattenables.toList flatMap { _.tokens }

}

case class GeneralTokens(val toks: List[Token]) extends AstNode with TypeElement with ExprElement {
  lazy val tokens = flatten(toks)
}

case class Refinement(lbrace: Token, refineStatSeq: StatSeq, rbrace: Token) extends AstNode with TypeElement {
  lazy val tokens = flatten(lbrace, refineStatSeq, rbrace)
}

case class TypeParamClause(contents: List[TypeElement]) extends AstNode with TypeElement {
  //require(!contents.isEmpty)
  lazy val tokens = flatten(contents)
}

case class Annotation(at: Token, annotationType: Type, argumentExprss: List[ArgumentExprs], newlineOption: Option[Token]) extends TypeElement with ExprElement {
  lazy val tokens = flatten(at, annotationType, argumentExprss, newlineOption)
}

case class InfixTypeConstructor(id: Token) extends AstNode with TypeElement {
  lazy val tokens = flatten(id)
}

sealed trait TypeElement extends AstNode

case class Type(contents: List[TypeElement]) extends AstNode with TypeElement {
  //require(!contents.isEmpty)
  lazy val tokens = flatten(contents)
}

case class VarianceTypeElement(id: Token) extends AstNode with TypeElement {
  lazy val tokens = flatten(id)
}

case class VarargsTypeElement(star: Token) extends AstNode with TypeElement {
  lazy val tokens = flatten(star)
}
case class CallByNameTypeElement(arrow: Token) extends AstNode with TypeElement {
  lazy val tokens = flatten(arrow)
}

sealed trait ExprElement extends AstNode

case class Expr(contents: List[ExprElement]) extends AstNode with ExprElement with Stat with Enumerator with XmlContents with ImportExpr {
  lazy val tokens = flatten(contents)
}

case class ParenExpr(lparen: Token, contents: List[ExprElement], rparen: Token) extends ExprElement {
  lazy val tokens = flatten(lparen, contents, rparen)
}

case class PrefixExprElement(id: Token) extends AstNode with ExprElement {
  lazy val tokens = flatten(id)
}

case class InfixExprElement(id: Token) extends AstNode with ExprElement {
  lazy val tokens = flatten(id)
}

case class PostfixExprElement(id: Token) extends AstNode with ExprElement {
  lazy val tokens = flatten(id)
}

case class TypeExprElement(contents: List[TypeElement]) extends AstNode with ExprElement {
  //require(!contents.isEmpty)
  lazy val tokens = flatten(contents)
}

trait ArgumentExprs extends ExprElement

case class BlockArgumentExprs(contents: List[ExprElement]) extends ArgumentExprs {
  lazy val tokens = flatten(contents)
}

case class ParenArgumentExprs(lparen: Token, contents: List[ExprElement], rparen: Token) extends ArgumentExprs {
  lazy val tokens = flatten(lparen, contents, rparen)
}

case class IfExpr(ifToken: Token,
                  condExpr: CondExpr,
                  newlinesOpt: Option[Token],
                  body: Expr,
                  elseClause: Option[ElseClause]) extends AstNode with ExprElement {

  lazy val tokens = flatten(ifToken, condExpr, newlinesOpt, body, elseClause)

}

case class ElseClause(semiOpt: Option[Token], elseToken: Token, elseBody: Expr) extends AstNode {
  lazy val tokens = flatten(semiOpt, elseToken, elseBody)
}

case class BlockExpr(lbrace: Token, caseClausesOrStatSeq: Either[CaseClauses, StatSeq], rbrace: Token) extends AstNode with ExprElement {
  lazy val tokens = flatten(lbrace, caseClausesOrStatSeq, rbrace)
}

case class CondExpr(lparen: Token, condition: Expr, rparen: Token) extends AstNode {
  lazy val tokens = flatten(lparen, condition, rparen)
}

case class WhileExpr(whileToken: Token, condExpr: CondExpr, newlinesOpt: Option[Token], body: Expr) extends AstNode with ExprElement {
  lazy val tokens = flatten(whileToken, condExpr, newlinesOpt, body)
}

case class DoExpr(doToken: Token, body: Expr, statSepOpt: Option[Token], whileToken: Token, condExpr: CondExpr) extends AstNode with ExprElement {
  lazy val tokens = flatten(doToken, body, statSepOpt, whileToken, condExpr)
}

case class ForExpr(forToken: Token,
                   lParenOrBrace: Token,
                   enumerators: Enumerators,
                   rParenOrBrace: Token,
                   newlinesOption: Option[Token],
                   yieldOption: Option[Token],
                   body: Expr) extends AstNode with ExprElement {

  lazy val tokens = flatten(forToken, lParenOrBrace, enumerators, rParenOrBrace, newlinesOption, yieldOption, body)

}

sealed trait Enumerator extends AstNode

case class Enumerators(initialGenerator: Generator, rest: List[(Token, Enumerator)]) extends AstNode {
  lazy val tokens = flatten(initialGenerator, rest)
}

case class Generator(
  valOption: Option[Token],
  pattern: Expr,
  equalsOrArrowToken: Token,
  expr: Expr,
  guards: List[Guard]) extends AstNode with Enumerator {

  lazy val tokens = flatten(valOption, pattern, equalsOrArrowToken, expr, guards)

}

case class Guard(ifToken: Token, expr: Expr) extends AstNode with Enumerator {
  lazy val tokens = flatten(ifToken, expr)
}

case class TryExpr(tryToken: Token, body: Expr, catchClauseOption: Option[(Token, BlockExpr)], finallyClauseOption: Option[(Token, Expr)]) extends AstNode with ExprElement {
  lazy val tokens = flatten(tryToken, body, catchClauseOption, finallyClauseOption)
}

case class FullDefOrDcl(annotations: List[Annotation], modifiers: List[Modifier], defOrDcl: DefOrDcl) extends Stat {
  lazy val tokens = flatten(annotations, modifiers, defOrDcl)
}

case class CaseClause(caseToken: Token, pattern: Expr, guardOption: Option[Guard], arrow: Token, statSeq: StatSeq) extends AstNode {
  lazy val tokens = flatten(caseToken, pattern, guardOption, arrow, statSeq)
}

case class CaseClauses(caseClauses: List[CaseClause]) extends AstNode {
  //require(!caseClauses.isEmpty)
  lazy val tokens = flatten(caseClauses)
}

sealed trait DefOrDcl extends AstNode

case class TypeDefOrDcl(contents: List[TypeElement]) extends DefOrDcl {
  //require(!contents.isEmpty)
  lazy val tokens = flatten(contents)
}

case class PatDefOrDcl(valOrVarToken: Token,
                       pattern: Expr,
                       otherPatterns: List[(Token, Expr)],
                       typedOpt: Option[(Token, Type)],
                       equalsClauseOption: Option[(Token, Expr)]) extends DefOrDcl {

  lazy val tokens = flatten(valOrVarToken, pattern, otherPatterns, typedOpt, equalsClauseOption)

}

sealed trait FunBody extends AstNode
case class ProcFunBody(newlineOpt: Option[Token], bodyBlock: BlockExpr) extends FunBody {
  lazy val tokens = flatten(newlineOpt, bodyBlock)
}
case class ExprFunBody(equals: Token, body: Expr) extends FunBody {
  lazy val tokens = flatten(equals, body)
}

case class ParamClauses(newlineOpt: Option[Token], paramClausesAndNewlines: List[(ParamClause, Option[Token])]) extends AstNode {
  lazy val tokens = flatten(newlineOpt, paramClausesAndNewlines)
}

case class ParamClause(lparen: Token, implicitOption: Option[Token], firstParamOption: Option[Param], otherParams: List[(Token, Param)], rparen: Token) extends AstNode {
  lazy val tokens = flatten(lparen, implicitOption, firstParamOption, otherParams, rparen)
}

case class Param(annotations: List[Annotation], modifiers: List[Modifier], valOrVarOpt: Option[Token], id: Token, paramTypeOpt: Option[(Token, Type)], defaultValueOpt: Option[(Token, Expr)]) extends AstNode {
  lazy val tokens = flatten(annotations, modifiers, valOrVarOpt, id, paramTypeOpt, defaultValueOpt)
}

case class FunDefOrDcl(defToken: Token,
                       nameToken: Token, // id or THIS
                       typeParamClauseOpt: Option[TypeParamClause],
                       paramClauses: ParamClauses,
                       returnTypeOpt: Option[(Token, Type)],
                       funBodyOpt: Option[FunBody]) extends DefOrDcl {

  lazy val tokens = flatten(defToken, nameToken, typeParamClauseOpt, paramClauses, returnTypeOpt, funBodyOpt)

}

case class TmplDef(markerTokens: List[Token],
                   name: Token,
                   typeParamClauseOpt: Option[TypeParamClause],
                   annotations: List[Annotation],
                   accessModifierOpt: Option[AccessModifier],
                   paramClausesOpt: Option[ParamClauses],
                   templateInheritanceSectionOpt: Option[TemplateInheritanceSection],
                   templateBodyOption: Option[TemplateBody]) extends DefOrDcl {
  //require(markerTokens.size <= 2)
  lazy val tokens = flatten(markerTokens, name, typeParamClauseOpt, annotations, accessModifierOpt, paramClausesOpt, templateInheritanceSectionOpt, templateBodyOption)

}

case class TemplateInheritanceSection(extendsOrSubtype: Token,
                                      earlyDefsOpt: Option[EarlyDefs],
                                      templateParentsOpt: Option[TemplateParents]) extends AstNode {

  lazy val tokens = flatten(extendsOrSubtype, earlyDefsOpt, templateParentsOpt)

}

case class EarlyDefs(earlyBody: TemplateBody, withOpt: Option[Token]) extends AstNode {
  lazy val tokens = flatten(earlyBody, withOpt)
}

case class Template(earlyDefsOpt: Option[EarlyDefs], templateParentsOpt: Option[TemplateParents], templateBodyOpt: Option[TemplateBody]) extends ExprElement {
  lazy val tokens = flatten(earlyDefsOpt, templateParentsOpt, templateBodyOpt)
}

case class TemplateBody(newlineOpt: Option[Token], lbrace: Token, statSeq: StatSeq, rbrace: Token) extends AstNode {
  lazy val tokens = flatten(newlineOpt, lbrace, statSeq, rbrace)
}

sealed trait Stat extends AstNode

case class StatSeq(selfReferenceOpt: Option[(Expr, Token)],
                   firstStatOpt: Option[Stat],
                   otherStats: List[(Token, Option[Stat])]) extends AstNode with ExprElement {

  lazy val tokens = flatten(selfReferenceOpt, firstStatOpt, otherStats)

}

case class TemplateParents(type1: Type, argumentExprss: List[ArgumentExprs], withTypes: List[(Token, Type)]) extends AstNode {
  lazy val tokens = flatten(type1, argumentExprss, withTypes)
}

case class ImportClause(importToken: Token, importExpr: ImportExpr, otherImportExprs: List[(Token, ImportExpr)]) extends AstNode with Stat {
  lazy val tokens = flatten(importToken, importExpr, otherImportExprs)
}

sealed trait ImportExpr extends AstNode

case class BlockImportExpr(prefixExpr: Expr, importSelectors: ImportSelectors) extends ImportExpr {
  lazy val tokens = flatten(prefixExpr, importSelectors)
}

case class ImportSelectors(lbrace: Token, firstImportSelector: Expr, otherImportSelectors: List[(Token, Expr)], rbrace: Token) extends AstNode {
  lazy val tokens = flatten(lbrace, firstImportSelector, otherImportSelectors, rbrace)
}

case class PackageBlock(packageToken: Token, name: List[Token], newlineOpt: Option[Token], lbrace: Token, topStats: StatSeq, rbrace: Token) extends Stat {
  lazy val tokens = flatten(packageToken, name, newlineOpt, lbrace, topStats, rbrace)
}

case class PrePackageBlock(name: List[Token], newlineOpt: Option[Token], lbrace: Token, topStats: StatSeq, rbrace: Token) {
  def complete(packageToken: Token) = PackageBlock(packageToken, name, newlineOpt, lbrace, topStats, rbrace)
}

case class PackageStat(packageToken: Token, name: List[Token]) extends Stat {
  lazy val tokens = flatten(packageToken, name)
}

sealed trait Modifier extends AstNode

case class SimpleModifier(token: Token) extends Modifier {
  lazy val tokens = flatten(token)
}

case class AccessModifier(privateOrProtected: Token, accessQualifierOpt: Option[AccessQualifier]) extends Modifier {
  lazy val tokens = flatten(privateOrProtected, accessQualifierOpt)
}
case class AccessQualifier(lbracket: Token, thisOrId: Token, rbracket: Token) extends AstNode {
  lazy val tokens = flatten(lbracket, thisOrId, rbracket)
}

case class CompilationUnit(topStats: StatSeq) extends AstNode {
  lazy val tokens = flatten(topStats)
}

case class AnonymousFunctionStart(parameters: List[ExprElement], arrow: Token) extends ExprElement {
  lazy val tokens = flatten(parameters, arrow)
}

case class AnonymousFunction(parameters: List[ExprElement], arrow: Token, body: List[ExprElement]) extends ExprElement {
  lazy val tokens = flatten(parameters, arrow, body)
}

sealed trait XmlExprElement extends ExprElement

case class XmlStartTag(startOpen: Token, name: Token, attributes: List[(Option[Token], XmlAttribute)], whitespaceOption: Option[Token], tagClose: Token) extends XmlExprElement {
  lazy val tokens = flatten(startOpen, name, attributes, whitespaceOption, tagClose)
}
case class XmlAttribute(name: Token, whitespaceOption: Option[Token], equals: Token, whitespaceOption2: Option[Token], valueOrEmbeddedScala: Either[Token, Expr]) extends XmlExprElement {
  lazy val tokens = flatten(name, whitespaceOption, equals, whitespaceOption2, valueOrEmbeddedScala)
}
case class XmlEmptyElement(startOpen: Token, name: Token, attributes: List[(Option[Token], XmlAttribute)], whitespaceOption: Option[Token], emptyClose: Token) extends XmlElement {
  lazy val tokens = flatten(startOpen, name, attributes, whitespaceOption, emptyClose)
}
case class XmlEndTag(endOpen: Token, name: Token, whitespaceOption: Option[Token], tagClose: Token) extends XmlExprElement {
  lazy val tokens = flatten(endOpen, name, whitespaceOption, tagClose)
}

sealed trait XmlElement extends XmlContents

case class XmlNonEmptyElement(startTag: XmlStartTag, contents: List[XmlContents], endTag: XmlEndTag) extends XmlElement {
  lazy val tokens = flatten(startTag, contents, endTag)
}

sealed trait XmlContents extends XmlExprElement

case class XmlPCDATA(token: Token) extends XmlContents { lazy val tokens = flatten(token) }
case class XmlCDATA(token: Token) extends XmlContents { lazy val tokens = flatten(token) }
case class XmlComment(token: Token) extends XmlContents { lazy val tokens = flatten(token) }
case class XmlUnparsed(token: Token) extends XmlContents { lazy val tokens = flatten(token) }
case class XmlProcessingInstruction(token: Token) extends XmlContents { lazy val tokens = flatten(token) }

case class XmlExpr(first: XmlContents, otherElements: List[XmlContents]) extends ExprElement {
  lazy val tokens = flatten(first, otherElements)
}

// Not an AST node, used as an intermediate structure during parsing
case class TemplateOpt(templateInheritanceSectionOpt: Option[TemplateInheritanceSection], templateBodyOpt: Option[TemplateBody])

