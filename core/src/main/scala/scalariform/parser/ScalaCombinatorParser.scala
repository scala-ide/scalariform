package scalariform.parser

import scala.util.parsing.input._
import scala.util.parsing.combinator._

import scalariform.lexer.Tokens._
import scalariform.lexer._
import scalariform.utils.Utils._

class ScalaCombinatorParser extends Parsers {

  type Elem = Token

  def when[T](condition: Boolean, parser: Parser[T]): Parser[T] =
    if (condition) parser else failure("condition did not hold")

  def pairRep[U, T](parser1: Parser[U], parser2: Parser[T]): Parser[List[(U, T)]] = rep(parser1 ~ parser2 ^^ { case result1 ~ result2 => (result1, result2) })
  def pairOpt[U, T](parser1: Parser[U], parser2: Parser[T]): Parser[Option[(U, T)]] = opt(parser1 ~ parser2 ^^ { case result1 ~ result2 => (result1, result2) })

  class ParserExtra[+T](parser1: Parser[T]) {

    def whenFollowedBy[U](parser2: Parser[U]): Parser[T] = parser1 <~ guard(parser2)

    def |/[U](parser2: Parser[U]): Parser[T Either U] =
      parser1 ^^ { Left(_) } | parser2 ^^ { Right(_) }

    def dependantParser[U](mkParser: (T) ⇒ Parser[U]): Parser[T ~ U] =
      parser1 into { t ⇒
        mkParser(t).map(u ⇒ new ~(t, u)).named("dependantParser")
      }

  }
  implicit def parserToParserExtra[T](parser: Parser[T]): ParserExtra[T] = new ParserExtra[T](parser)
  implicit def tokenTypeToParserExtra(tokenType: TokenType): ParserExtra[Token] =
    new ParserExtra[Token](tokenTypeToParser(tokenType))

  implicit def tokenTypeToParser(tokenType: TokenType): Parser[Token] = elem(tokenType.toString, _.getType == tokenType)

  trait Flattenable { def tokens: List[Token] }
  case class Tokens(val tokens: List[Token]) extends Flattenable
  def flatten[T <% Flattenable]: (T ⇒ List[Token]) = _.tokens
  implicit def tokenToFlattenable(token: Token): Flattenable = Tokens(List(token))
  implicit def pairToFlattenable[A <% Flattenable, B <% Flattenable](pair: A ~ B): Flattenable =
    Tokens(pair._1.tokens ::: pair._2.tokens)
  implicit def optionToFlattenable[T <% Flattenable](option: Option[T]): Flattenable = option.toList
  implicit def listToFlattenable[T <% Flattenable](list: List[T]): Flattenable = Tokens(list flatMap { _.tokens })

  // -- Types -----------------------------------------------------------------------

  trait TypeElementFlattenable { def elements: List[TypeElement] }
  case class TypeElements(val elements: List[TypeElement]) extends TypeElementFlattenable
  def typeElementFlatten[T <% TypeElementFlattenable]: (T ⇒ List[TypeElement]) = _.elements
  def typeElementFlatten2[T <% TypeElementFlattenable](t: T): List[TypeElement] = t.elements
  implicit def tokenToTypeFlattenable(token: Token): TypeElementFlattenable = GeneralTokens(List(token))
  implicit def listOfTokenToTypeFlattenable(tokens: List[Token]): TypeElementFlattenable = GeneralTokens(tokens)
  implicit def typeElementToTypeFlattenable(typeElement: TypeElement): TypeElementFlattenable = TypeElements(List(typeElement))
  implicit def pairToTypeFlattenable[A <% TypeElementFlattenable, B <% TypeElementFlattenable](pair: A ~ B): TypeElementFlattenable =
    TypeElements(pair._1.elements ::: pair._2.elements)
  implicit def eitherToTypeFlattenable[A <% TypeElementFlattenable, B <% TypeElementFlattenable](either: Either[A, B]) = TypeElements(either match {
    case Left(x) ⇒ x.elements
    case Right(x) ⇒ x.elements
  })
  implicit def optionToTypeFlattenable[T <% TypeElementFlattenable](option: Option[T]): TypeElementFlattenable = option.toList
  implicit def listToTypeFlattenable[T <% TypeElementFlattenable](list: List[T]): TypeElementFlattenable = TypeElements(list flatMap { _.elements })

  lazy val id: Parser[Token] = VARID | OTHERID | PLUS | MINUS | STAR | PIPE | TILDE | EXCLAMATION

  lazy val nonStarId: Parser[Token] = VARID | OTHERID | PLUS | MINUS |/* STAR | */PIPE | TILDE | EXCLAMATION

  lazy val nonPipeId: Parser[Token] = VARID | OTHERID | PLUS | MINUS | STAR |/* PIPE | */TILDE | EXCLAMATION

  lazy val statSep: Parser[Token] = NEWLINE | NEWLINES | SEMI

  def path(thisOK: Boolean, typeOK: Boolean): Parser[List[Token]] = { // TODO: Enforce thisOK = false
    val thisBranch = THIS ~ opt(DOT ~ selectors(typeOK)) ^^ flatten
    val superBranch = SUPER ~ mixinQualifierOpt ~ DOT ~ id ~ opt(DOT ~ selectors(typeOK)) ^^ flatten
    val idBranch = {
      val idThisBranch = THIS ~ opt(DOT ~ selectors(typeOK)) ^^ flatten
      val idSuperBranch = SUPER ~ mixinQualifierOpt ~ DOT ~ id ~ opt(DOT ~ selectors(typeOK)) ^^ flatten
      val idIdBranch = selectors(typeOK)
      id ~ opt(DOT ~ (idThisBranch | idSuperBranch | idIdBranch)) ^^ flatten
    }
    (thisBranch | superBranch | idBranch) ^^ flatten
  }

  def selectors(typeOK: Boolean): Parser[List[Token]] = {
    lazy val idDotSelectors = id ~ rep(DOT ~ selectors(typeOK)) ^^ flatten
    if (typeOK)
      TYPE ^^ flatten | idDotSelectors
    else
      idDotSelectors
  }

  lazy val mixinQualifierOpt: Parser[List[Token]] = opt(LBRACKET ~ id ~ RBRACKET) ^^ flatten
  lazy val stableId = path(thisOK = false, typeOK = false)
  lazy val qualId = selectors(typeOK = false)

  lazy val literal: Parser[Token] = CHARACTER_LITERAL | INTEGER_LITERAL | FLOATING_POINT_LITERAL | STRING_LITERAL | SYMBOL_LITERAL | TRUE | FALSE | NULL

  lazy val newlineOpt: Parser[Option[Token]] = opt(NEWLINE)
  lazy val newlinesOpt: Parser[Option[Token]] = opt(NEWLINE | NEWLINES)

  lazy val typedOpt: Parser[Option[(Token, Type)]] = pairOpt(COLON, typ_)

  def types(p: Parser[List[TypeElement]]) = p ~ rep(COMMA ~ p) ^^ typeElementFlatten

  lazy val typ_ = typ(isPattern = false)

  def typ(isPattern: Boolean): Parser[Type] = {
    lazy val argType = if (isPattern) patternArgType else funcArgType
    lazy val typeStack = simpleTypeRest(isPattern) ~ annotTypeRest ~ compoundTypeRest(isPattern) ~ infixTypeRest(isPattern)
    lazy val earlyClose = RPAREN ~ ARROW ~ typ(isPattern)
    lazy val lateClose = types(argType) ~ RPAREN ~ (ARROW ~ typ(isPattern) |/ typeStack)
    lazy val typMain = LPAREN ~ (earlyClose |/ lateClose) |/ infixType(isPattern)
    lazy val typRest = ARROW ~ typ(isPattern) |/ FORSOME ~ refinement
    lazy val typAll = typMain ~ opt(typRest) ^^ typeElementFlatten
    typAll ^^ { Type(_) } // TODO: Why not merge with previous?
  }

  def infixType(isPattern: Boolean) = compoundType(isPattern) ~ infixTypeRest(isPattern) ^^ typeElementFlatten
  def infixTypeRest(isPattern: Boolean): Parser[List[TypeElement]] = {
    lazy val infixId = nonStarId ^^ { InfixTypeConstructor(_) }
    opt(infixId dependantParser {
      case InfixTypeConstructor(token) ⇒ {
        val isLeftAssociative = !token.getText.endsWith(":")
        lazy val leftAssociativeRemainder = compoundType(isPattern) ~ infixTypeRest(isPattern) ^^ typeElementFlatten
        lazy val rightAssociativeRemainder = infixType(isPattern) ^^ typeElementFlatten
        newlineOpt ~ (if (isLeftAssociative) leftAssociativeRemainder else rightAssociativeRemainder)
      }
    }) ^^ typeElementFlatten
  }

  def compoundType(isPattern: Boolean) = (annotType(isPattern) |/ guard(LBRACE) ~> success(List[TypeElement]())) ~ compoundTypeRest(isPattern) ^^ typeElementFlatten
  def compoundTypeRest(isPattern: Boolean) = rep(WITH ~ annotType(isPattern)) ~ opt(newlineOpt ~ refinement) ^^ typeElementFlatten
  def annotType(isPattern: Boolean) = simpleType(isPattern) ~ annotTypeRest ^^ typeElementFlatten
  def annotTypeRest = annotations(false, false) ^^ typeElementFlatten

  def simpleType(isPattern: Boolean): Parser[List[TypeElement]] = {
    lazy val argType = if (isPattern) patternArgType else (typ_ ^^ { List(_) })
    lazy val tupleType = LPAREN ~ types(argType) ~ RPAREN
    lazy val uscoreWildcard = USCORE ~ wildcardType
    (tupleType |/ uscoreWildcard |/ path(thisOK = false, typeOK = true)) ~ simpleTypeRest(isPattern) ^^ typeElementFlatten
  }

  def simpleTypeRest(isPattern: Boolean): Parser[List[TypeElement]] = {
    val hashTypeSuffix = HASH ~ id ~ simpleTypeRest(isPattern) ^^ typeElementFlatten
    val typeArgsSuffix = typeArgs(isPattern) ~ simpleTypeRest(isPattern) ^^ typeElementFlatten
    opt(hashTypeSuffix | typeArgsSuffix) ^^ typeElementFlatten
  }

  val wildcardType = typeBounds

  def typeArgs(isPattern: Boolean) = {
    val argType = if (isPattern) patternArgType else (typ_ ^^ { List(_) })
    LBRACKET ~ types(argType) ~ RBRACKET ^^ typeElementFlatten
  }

  lazy val patternArgType = (USCORE ~ opt(wildcardType) |/ /* id |/ */typ_) ^^ typeElementFlatten // TODO: is id needed ?
  lazy val funcArgType = {
    lazy val varargs = typ_ ~ opt(STAR ^^ { VarargsTypeElement(_) })
    lazy val callByName = (ARROW ^^ { CallByNameTypeElement(_) }) ~ typ_
    (callByName |/ varargs) ^^ typeElementFlatten
  }

  // -- Expressions -----------------------------------------------------------------------

  trait ExprElementFlattenable { def elements: List[ExprElement] }
  case class ExprElements(val elements: List[ExprElement]) extends ExprElementFlattenable
  def exprElementFlatten[T <% ExprElementFlattenable]: (T ⇒ List[ExprElement]) = t ⇒ { exprElementFlatten2(t) }
  def exprElementFlatten2[T <% ExprElementFlattenable](t: T): List[ExprElement] = groupGeneralTokens(t.elements)
  def groupGeneralTokens(xs: List[ExprElement]): List[ExprElement] = {
    val eq = (x: ExprElement, y: ExprElement) ⇒
      (x, y) match {
      case (GeneralTokens(_), GeneralTokens(_)) ⇒ true
      case _ ⇒ false
    }
    val groups = groupBy(eq, xs)
    groups map { group ⇒
      {
        val item = group.head
        var tokens: List[Token] = List()
        if (item.isInstanceOf[GeneralTokens]) {
          for (groupItem ← group)
            tokens = tokens ::: groupItem.asInstanceOf[GeneralTokens].tokens
          val result = GeneralTokens(tokens)
          result
        } else
          item
      }
    }
  }

  implicit def tokenToExprFlattenable(token: Token): ExprElementFlattenable = GeneralTokens(List(token))
  implicit def listOfTokenToExprFlattenable(tokens: List[Token]): ExprElementFlattenable = GeneralTokens(tokens)
  implicit def exprToExprFlattenable(expr: Expr): ExprElementFlattenable = expr.contents
  implicit def exprElementToExprFlattenable(exprElement: ExprElement): ExprElementFlattenable = ExprElements(List(exprElement))
  implicit def pairToExprFlattenable[A <% ExprElementFlattenable, B <% ExprElementFlattenable](pair: A ~ B): ExprElementFlattenable =
    ExprElements(pair._1.elements ::: pair._2.elements)
  implicit def ordinaryPairToExprFlattenable[A <% ExprElementFlattenable, B <% ExprElementFlattenable](pair: (A, B)): ExprElementFlattenable =
    ExprElements(pair._1.elements ::: pair._2.elements)
  implicit def eitherToExprFlattenable[A <% ExprElementFlattenable, B <% ExprElementFlattenable](either: Either[A, B]) = ExprElements(either match {
    case Left(x) ⇒ x.elements
    case Right(x) ⇒ x.elements
  })
  implicit def optionToExprFlattenable[T <% ExprElementFlattenable](option: Option[T]): ExprElementFlattenable = option.toList
  implicit def listToExprFlattenable[T <% ExprElementFlattenable](list: List[T]): ExprElementFlattenable = ExprElements(list flatMap { _.elements })

  abstract sealed class Location
  case object Local extends Location
  case object InBlock extends Location
  case object InTemplate extends Location

  lazy val equalsExpr = EQUALS ~ expr
  lazy val condExpr = LPAREN ~ expr ~ RPAREN ^^ { // | LPAREN // WTF?
    case lparen ~ expr ~ rparen ⇒ CondExpr(lparen, expr, rparen)
  }
  lazy val expr: Parser[Expr] = expr0(Local)

  lazy val tryExpr: Parser[TryExpr] = {
    lazy val blockBody: Parser[BlockExpr] = LBRACE ~ block ~ RBRACE ^^ { case lbrace ~ block ~ rbrace ⇒ BlockExpr(lbrace, Right(block), rbrace) }
    lazy val otherExprBody = LPAREN ~ expr ~ RPAREN |/ expr
    lazy val body: Parser[Expr] = (blockBody |/ otherExprBody) ^^ { x ⇒ Expr(exprElementFlatten2(x)) }
    lazy val catchBody = LBRACE ~ caseClauses ~ RBRACE ^^ { case lbrace ~ caseClauses ~ rbrace ⇒ BlockExpr(lbrace, Left(caseClauses), rbrace) }
    TRY ~ body ~ pairOpt(CATCH, catchBody) ~ pairOpt(FINALLY, expr) ^^ {
      case tryToken ~ body ~ catchClauseOption ~ finallyClauseOption ⇒ TryExpr(tryToken, body, catchClauseOption, finallyClauseOption)
    }
  }

  def expr0(location: Location): Parser[Expr] = {
    // TODO: Cleanup: 
    lazy val ascription = (COLON ~ (USCORE ~ STAR |/ guard(AT) ~> annotations(skipNewLines = false, requireOneArgList = false) |/ (if (location == Local) typ_ ^^ { type_ ⇒ TypeExprElement(List(type_)) } else infixType(isPattern = false) ^^ { TypeExprElement(_) }))) ^^ exprElementFlatten
    lazy val matchExpr = MATCH ~ (LBRACE ~ caseClauses ~ RBRACE ^^ { case lbrace ~ caseClauses ~ rbrace ⇒ BlockExpr(lbrace, Left(caseClauses), rbrace) }) ^^ exprElementFlatten
    lazy val other = postfixExpr ~ opt(EQUALS ~ expr/* TODO: only if postfixExpr is Ident, Select or Apply */ |/ ascription |/ matchExpr) ^^ exprElementFlatten
    lazy val res = (ifExpr |/ tryExpr |/ whileExpr |/ doExpr |/ forExpr |/ returnExpr |/ throwExpr |/ IMPLICIT ~ implicitClosure(location) |/ other) ^^ exprElementFlatten
    lazy val postArrow = if (location == InBlock) block ^^ exprElementFlatten else expr ^^ exprElementFlatten
    //lazy val arrowSuffix = ARROW ~ postArrow ^^ exprElementFlatten
    lazy val typeParamList = LPAREN ~ opt(id ~ ascription ~ rep(COMMA ~ id ~ ascription)) ~ RPAREN ^^ exprElementFlatten
    lazy val anonymousFunctionStart = typeParamList ~ ARROW ^^ { case typeParamList ~ arrow ⇒ AnonymousFunctionStart(typeParamList, arrow) }
    lazy val potentialAnonymousFunction = res ~ opt(when(location != InTemplate, ARROW ~ postArrow)) ^^ {
      case res ~ Some(arrow ~ postArrow) ⇒ exprElementFlatten2((AnonymousFunctionStart(res, arrow), postArrow))
      case res ~ None ⇒ res
    }
    lazy val res1 = (anonymousFunctionStart ~ postArrow |/ potentialAnonymousFunction) ^^ exprElementFlatten
    res1 ^^ { Expr(_) }
  }

  lazy val ifExpr = {
    val elseClause = opt(SEMI) ~ ELSE ~ expr ^^ {
      case semiOpt ~ elseToken ~ expr ⇒ ElseClause(semiOpt, elseToken, expr)
    }
    IF ~ condExpr ~ newlinesOpt ~ expr ~ opt(elseClause) ^^ {
      case ifToken ~ condExpr ~ newlinesOpt ~ body ~ elseClauseOption ⇒ IfExpr(ifToken, condExpr, newlinesOpt, body, elseClauseOption)
    }
  }

  lazy val whileExpr = WHILE ~ condExpr ~ newlinesOpt ~ expr ^^ {
    case whileToken ~ condExpr ~ newlinesOpt ~ body ⇒ WhileExpr(whileToken, condExpr, newlinesOpt, body)
  }

  lazy val doExpr = DO ~ expr ~ opt(statSep) ~ WHILE ~ condExpr ^^ {
    case doToken ~ body ~ statSepOpt ~ whileToken ~ condExpr ⇒ DoExpr(doToken, body, statSepOpt, whileToken, condExpr)
  }

  lazy val forExpr = {
    val bracketedEnumerators = LBRACE ~ enumerators ~ RBRACE | LPAREN ~ enumerators ~ RPAREN
    FOR ~ bracketedEnumerators ~ newlinesOpt ~ opt(YIELD) ~ expr ^^ {
      case forToken ~(lParenOrBrace ~ enumerators ~ rParenOrBrace) ~ newlinesOption ~ yieldOption ~ body ⇒
        ForExpr(forToken, lParenOrBrace, enumerators, rParenOrBrace, newlinesOption, yieldOption, body)
    }
  }

  lazy val returnExpr = RETURN ~ opt(expr) ^^ exprElementFlatten

  lazy val throwExpr = THROW ~ expr ^^ exprElementFlatten

  def implicitClosure(location: Location) = id ~ ARROW ~ (if (location == InBlock) block ^^ exprElementFlatten else expr ^^ exprElementFlatten) ^^ exprElementFlatten

  lazy val postfixExpr = { // TODO: Clean this up
    val remainder = newlineOpt ~ prefixExpr ^^ exprElementFlatten
    val postfixPart = id ~ opt(remainder) ^^ {
      case (identifier ~ Some(other)) ⇒ InfixExprElement(identifier) :: other
      case (identifier ~ None) ⇒ List(PostfixExprElement(identifier))
    }
    prefixExpr ~ rep(postfixPart) ^^ exprElementFlatten
  }

  lazy val prefixExpr = {
    val withMinusPrefix = (MINUS ^^ { PrefixExprElement(_) }) ~ (INTEGER_LITERAL |/ simpleExpr)
    val withOtherPrefix = ((PLUS | TILDE | EXCLAMATION) ^^ { PrefixExprElement(_) }) ~ simpleExpr
    (withMinusPrefix |/ withOtherPrefix |/ simpleExpr) ^^ exprElementFlatten
  }

  lazy val simpleExpr = {
    val first = (literal
      |/ xmlExpr
      |/ path(thisOK = true, typeOK = false)
      |/ USCORE
      |/ LPAREN ~ opt(expr ~ rep(COMMA ~ expr)) ~ RPAREN
      ) ~ simpleExprRest(canApply = true) ^^ exprElementFlatten // TODO: Check simpleExprRest applicability
    val blockOrNewTemplate = (blockExpr |/ NEW ~ template(isTrait = false)) ^^ exprElementFlatten
    val second = blockOrNewTemplate ~ simpleExprRest(canApply = false) ^^ exprElementFlatten
    (first |/ second) ^^ exprElementFlatten
  }

  def simpleExprRest(canApply: Boolean): Parser[List[ExprElement]] = {
    opt(
      DOT ~ id ~ simpleExprRest(canApply = true)
      |/ (typeArgs(isPattern = false) ^^ { TypeExprElement(_) }) ~ simpleExprRest(canApply = true) // TODO: Check grammar for condition
      |/ (when(canApply, (opt(NEWLINE whenFollowedBy LBRACE) ~ argumentExprs) ~ simpleExprRest(canApply = true)))
      |/ (USCORE ^^ { PostfixExprElement(_) })
      ) ^^ exprElementFlatten
  }

  lazy val argumentExprs: Parser[ArgumentExprs] = (blockExpr |/ LPAREN ~ opt(expr ~ rep(COMMA ~ expr)) ~ RPAREN) ^^ { x ⇒ ArgumentExprs(exprElementFlatten2(x)) }

  lazy val blockExpr = LBRACE ~ (caseClauses |/ block) ~ RBRACE ^^ { case (lbrace ~ body ~ rbrace) ⇒ BlockExpr(lbrace, body, rbrace) }
  lazy val block: Parser[StatSeq] = blockStatSeq

  lazy val caseClauses: Parser[CaseClauses] = {
    lazy val caseBlock = ARROW ~ block
    lazy val caseClause: Parser[CaseClause] = CASE ~ pattern ~ guard ~ caseBlock ^^ {
      case caseToken ~ pattern ~ guardOption ~(arrow ~ blockStatSeq) ⇒ CaseClause(caseToken, pattern, guardOption, arrow, blockStatSeq)
    }
    rep1(caseClause) ^^ { CaseClauses(_) }
  }

  lazy val guard: Parser[Option[Guard]] = {
    val actualGuard = IF ~ postfixExpr ^^ { case ifToken ~ postfixExpr ⇒ Guard(ifToken, Expr(postfixExpr)) }
    opt(actualGuard)
  }

  lazy val enumerators: Parser[Enumerators] = {
    val enumerator: Parser[Enumerator] = (guard(IF) ~> guard ^^ { _.get }) | generator(eqOK = true) | expr
    generator(eqOK = false) ~ pairRep(statSep, enumerator) ^^ {
      case initialGenerator ~ rest ⇒ Enumerators(initialGenerator, rest)
    }
  }

  def generator(eqOK: Boolean): Parser[Generator] = {
    opt(VAL) ~ pattern1(seqOK = false) ~ (when(eqOK, EQUALS) | LARROW) ~ expr ~ guard ^^ {
      case valOption ~ pattern ~ equalsOrArrowToken ~ expr ~ guard ⇒ Generator(valOption, pattern, equalsOrArrowToken, expr, guard)
    }
  }

  // -- Patterns -----------------------------------------------------------------------

  def patterns(seqOK: Boolean) = pattern(seqOK) ~ rep(COMMA ~ pattern(seqOK)) ^^ exprElementFlatten
  lazy val pattern: Parser[Expr] = pattern(seqOK = false)
  def pattern(seqOK: Boolean): Parser[Expr] = pattern1(seqOK) ~ rep((PIPE ^^ { InfixExprElement(_) }) ~ pattern1(seqOK)) ^^ { x ⇒ Expr(exprElementFlatten2(x)) }
  def pattern1(seqOK: Boolean) = {
    val typeAscription = COLON ~ (compoundType(isPattern = true) ^^ { typeElements ⇒ TypeExprElement(typeElements) })
    pattern2(seqOK) ~ opt(typeAscription) ^^
      { x ⇒ Expr(exprElementFlatten2(x)) } // TODO: condition on pattern2
  }
  def pattern2(seqOK: Boolean) = pattern3(seqOK) ~ opt(AT ~ pattern3(seqOK)) ^^ { x ⇒ Expr(exprElementFlatten2(x)) } // TODO: condition
  def pattern3(seqOK: Boolean) = {
    val rest = when(seqOK, STAR) |/ rep(nonPipeId ~ simplePattern(seqOK))
    simplePattern(seqOK) ~ rest ^^ exprElementFlatten
  }

  def simplePattern(seqOK: Boolean) = {
    val simplePattern0 = MINUS ~ (INTEGER_LITERAL | FLOATING_POINT_LITERAL) ^^ exprElementFlatten
    val simplePattern1 = stableId ~ opt(argumentPatterns) ^^ exprElementFlatten
    val simplePattern2 = LPAREN ~ opt(patterns(seqOK = false)) ~ RPAREN ^^ exprElementFlatten
    (simplePattern0 |/ simplePattern1 |/ USCORE |/ literal |/ simplePattern2 |/ xmlPattern) ^^ exprElementFlatten
  }

  lazy val argumentPatterns = LPAREN ~ opt(patterns(seqOK = true)) ~ RPAREN ^^ exprElementFlatten

  // -- Modifiers and annotations -------------------------------------------------------

  lazy val accessQualifierOpt: Parser[Option[AccessQualifier]] = {
    opt(LBRACKET ~ (THIS | id) ~ RBRACKET) ^^ { resultOpt ⇒
      resultOpt map { case lbracket ~ thisOrId ~ rbracket ⇒ AccessQualifier(lbracket, thisOrId, rbracket) }
    }
  }
  lazy val accessModifierOpt: Parser[Option[AccessModifier]] = {
    opt((PRIVATE | PROTECTED) ~ accessQualifierOpt) ^^ { resultOpt ⇒
      resultOpt map { case privateOrProtected ~ accessQualifierOpt ⇒ AccessModifier(privateOrProtected, accessQualifierOpt) }
    }
  }
  lazy val modifiers: Parser[List[Modifier]] = {
    val tokenModifier = (ABSTRACT | FINAL | SEALED | OVERRIDE | LAZY | IMPLICIT | NEWLINE) ^^ { SimpleModifier(_) }
    val privateProtected = (PRIVATE | PROTECTED) ~ accessQualifierOpt ^^ {
      case privateOrProtected ~ accessQualifierOpt ⇒ AccessModifier(privateOrProtected, accessQualifierOpt)
    }
    rep(tokenModifier | privateProtected)
  }
  lazy val localModifiers: Parser[List[Modifier]] = {
    val modifier = ABSTRACT | FINAL | SEALED | IMPLICIT | LAZY
    rep(modifier ^^ { SimpleModifier(_) })
  }

  def annotations(skipNewLines: Boolean, requireOneArgList: Boolean): Parser[List[Annotation]] = {
    val annotation = if (skipNewLines)
      AT ~ annotationExpr(requireOneArgList) ~ newlineOpt ^^
        { case at ~(annotationExpr ~ argumentExprss) ~ newlineOpt ⇒ Annotation(at, annotationExpr, argumentExprss, newlineOpt) }
    else
      AT ~ annotationExpr(requireOneArgList) ^^
        { case at ~(annotationExpr ~ argumentExprss) ⇒ Annotation(at, annotationExpr, argumentExprss, newlineOption = None) }
    rep(annotation)
  }

  def annotationExpr(requireOneArgList: Boolean) = {
    val annotationType = simpleType(isPattern = false) ^^ { Type(_) }
    val argumentExprss = if (requireOneArgList) argumentExprs ^^ { List(_) } else rep(argumentExprs)
    annotationType ~ argumentExprss
  }

  // -- Parameters ----------------------------------------------------------------------

  lazy val paramClauses: Parser[ParamClauses] = {

    val param: Parser[Param] = annotations(skipNewLines = false, requireOneArgList = false) ~ modifiers ~ opt(VAL | VAR) ~ id ~
      pairOpt(COLON, paramType) ~ pairOpt(EQUALS, expr) ^^ {
      case annotations ~ modifiers ~ valOrVarOpt ~ id ~ paramTypeOpt ~ defaultValueOpt ⇒ Param(annotations, modifiers, valOrVarOpt, id, paramTypeOpt, defaultValueOpt)
    }

    val paramClause: Parser[ParamClause] = LPAREN ~ opt(opt(IMPLICIT) ~ param ~ pairRep(COMMA, param)) ~ RPAREN ^^ {
      case lparen ~ None ~ rparen ⇒ ParamClause(lparen, None, None, Nil, rparen)
      case lparen ~ Some(implicitOption ~ param ~ otherParams) ~ rparen ⇒ ParamClause(lparen, implicitOption, Some(param), otherParams, rparen)
    }

    opt(NEWLINE whenFollowedBy LPAREN) ~ pairRep(paramClause, opt(NEWLINE whenFollowedBy LPAREN)) ^^ {
      case newlineOpt ~ paramClausesAndNewlines ⇒ ParamClauses(newlineOpt, paramClausesAndNewlines)
    }
  }

  lazy val paramType: Parser[Type] = {
    val callByNameType = ARROW ~ typ_
    val varargsType = typ_ ~ opt(STAR ^^ { VarargsTypeElement(_) })
    (callByNameType |/ varargsType) ^^ { x ⇒ Type(typeElementFlatten2(x)) }
  }

  lazy val typeParamClauseOpt: Parser[Option[TypeParamClause]] = {
    val typeParam: Parser[List[TypeElement]] = opt((PLUS | MINUS) ^^ { VarianceTypeElement(_) }) ~ (USCORE | id) ~ typeParamClauseOpt ~ typeBounds ~ rep(VIEWBOUND ~ typ_) ~ rep(COLON ~ typ_) ^^ typeElementFlatten
    val typeParamAnnotations = annotations(skipNewLines = true, requireOneArgList = false)
    val typeParamClause = newlineOpt ~ LBRACKET ~ typeParamAnnotations ~ typeParam ~ rep(COMMA ~ typeParamAnnotations ~ typeParam) ~ RBRACKET
    opt(typeParamClause ^^ { x ⇒ TypeParamClause(typeElementFlatten2(x)) })
  }

  lazy val typeBounds = opt(SUPERTYPE ~ typ_) ~ opt(SUBTYPE ~ typ_) ^^ typeElementFlatten

  // -- Defs ----------------------------------------------------------------------------

  lazy val importClause: Parser[ImportClause] = IMPORT ~ importExpr ~ pairRep(COMMA, importExpr) ^^ {
    case importToken ~ importExpr ~ otherImportExprs ⇒ ImportClause(importToken, importExpr, otherImportExprs)
  }

  lazy val importExpr: Parser[Expr] = {
    def loop: Parser[List[ExprElement]] = (USCORE |/ importSelectors |/ id ~ opt(DOT ~ loop)) ^^ exprElementFlatten
    def initialSelection = (THIS ~ DOT ~ id ~ DOT |/ id ~ DOT ~ opt(THIS ~ DOT ~ id ~ DOT)) ^^ exprElementFlatten
    initialSelection ~ loop ^^ { x ⇒ Expr(exprElementFlatten2(x)) }
  }

  lazy val importSelectors = LBRACE ~ importSelector ~ rep(COMMA ~ importSelector) ~ RBRACE ^^ exprElementFlatten

  lazy val importSelector = (USCORE |/ id ~ opt(ARROW ~ (USCORE | id))) ^^ exprElementFlatten

  lazy val defOrDcl: Parser[DefOrDcl] = patDefOrDcl | funDefOrDcl | typeDefOrDcl | tmplDef

  lazy val nonLocalDefOrDcl: Parser[FullDefOrDcl] = annotations(skipNewLines = true, requireOneArgList = false) ~ modifiers ~ defOrDcl ^^ {
    case annotations ~ modifiers ~ defOrDcl ⇒ FullDefOrDcl(annotations, modifiers, defOrDcl)
  }

  lazy val patDefOrDcl = {
    val equalsClauseOption = pairOpt(/* not optional if typedOpt is None */EQUALS, (USCORE ^^ { uscore ⇒ Expr(List(GeneralTokens(List(uscore)))) }/* extra conditions...*/ | expr))
    (VAL | VAR) ~ pattern2(seqOK = false) ~ pairRep(COMMA, pattern2(seqOK = false)) ~ typedOpt ~ equalsClauseOption ^^ {
      case valOrVarToken ~ pattern ~ otherPatterns ~ typedOpt ~ equalsClauseOption ⇒ {
        PatDefOrDcl(valOrVarToken, pattern, otherPatterns, typedOpt, equalsClauseOption)
      }
    }

  }

  lazy val funDefOrDcl: Parser[FunDefOrDcl] = {
    val constructorFunBody: Parser[FunBody] = newlineOpt ~ constrBlock ^^ { case newlineOpt ~ blockExpr ⇒ ProcFunBody(newlineOpt, blockExpr) } |
      EQUALS ~ constrExpr ^^ { case equalsToken ~ expr ⇒ ExprFunBody(equalsToken, expr) }
    val constructorDef = DEF ~ THIS ~ paramClauses ~ constructorFunBody ^^ {
      case defToken ~ nameToken ~ paramClauses ~ funBody ⇒ FunDefOrDcl(defToken, nameToken, None, paramClauses, None, Some(funBody))
    }

    val normalFunBody: Parser[FunBody] = newlineOpt ~ blockExpr ^^ { case newlineOpt ~ blockExpr ⇒ ProcFunBody(newlineOpt, blockExpr) } |
      equalsExpr ^^ { case equalsToken ~ expr ⇒ ExprFunBody(equalsToken, expr) }
    val normalDefOrDcl = DEF ~ id ~ typeParamClauseOpt ~ paramClauses ~/* TODO: newlineOpt? */typedOpt ~ opt(normalFunBody) ^^ {
      case defToken ~ nameToken ~ typeParamClauseOpt ~ paramClauses ~ returnTypeOpt ~ funBodyOpt ⇒
        FunDefOrDcl(defToken, nameToken, typeParamClauseOpt, paramClauses, returnTypeOpt, funBodyOpt)
    }

    constructorDef | normalDefOrDcl
  }

  lazy val constrExpr: Parser[Expr] = constrBlock ^^ { x ⇒ Expr(exprElementFlatten2(x)) } | selfInvocation

  lazy val selfInvocation: Parser[Expr] = {
    val guardedNewlineOpt: Parser[Option[Token]] = opt(NEWLINE whenFollowedBy LBRACE)
    THIS ~ guardedNewlineOpt ~ argumentExprs ~ guardedNewlineOpt ~ rep(argumentExprs ~ guardedNewlineOpt) ^^ { x ⇒ Expr(exprElementFlatten2(x)) }
  }

  lazy val constrBlock: Parser[BlockExpr] = {
    val constrBlockStatSeq: Parser[StatSeq] = selfInvocation ~ opt(statSep ~ blockStatSeq) ^^ {
      case selfInvocation ~ Some(statSep ~ blockStatSeq) ⇒ StatSeq(None, Some(selfInvocation), (statSep, blockStatSeq.firstStatOpt) :: blockStatSeq.otherStats)
      case selfInvocation ~ None ⇒ StatSeq(None, Some(selfInvocation), Nil)
    }
    LBRACE ~ constrBlockStatSeq ~ RBRACE ^^ { case (lbrace ~ blockStatSeq ~ rbrace) ⇒ BlockExpr(lbrace, Right(blockStatSeq), rbrace) }
  }

  lazy val typeDefOrDcl = {
    (TYPE ~ newlinesOpt ~ id ~ typeParamClauseOpt ~ (EQUALS ~ typ_ |/ typeBounds) ^^ typeElementFlatten) ^^ { TypeDefOrDcl(_) }
  }

  lazy val topLevelTmplDef: Parser[FullDefOrDcl] = annotations(skipNewLines = true, requireOneArgList = false) ~ modifiers ~ tmplDef ^^ {
    case annotations ~ modifiers ~ tmplDef ⇒ FullDefOrDcl(annotations, modifiers, tmplDef)
  }

  lazy val tmplDef: Parser[TmplDef] = classDef | objectDef

  lazy val classDef: Parser[TmplDef] = {
    val markerTokens: Parser[List[Token]] = TRAIT ^^ { List(_) } | opt(CASE) ~ CLASS ^^ { case caseOpt ~ classToken ⇒ caseOpt.toList ++ List(classToken) }
    markerTokens ~ id ~ typeParamClauseOpt ~ annotations(skipNewLines = false, requireOneArgList = true) ~ accessModifierOpt ~ paramClauses ~ templateOpt ^^ {
      case markerTokens ~ id ~ typeParamClauseOpt ~ annotations ~ accessModifierOpt ~ paramClauses ~ TemplateOpt(templateInheritanceSectionOpt, templateBodyOpt) ⇒
        TmplDef(markerTokens, id, typeParamClauseOpt, annotations, accessModifierOpt, Some(paramClauses), templateInheritanceSectionOpt, templateBodyOpt)
    }
  }

  lazy val objectDef: Parser[TmplDef] = {
    opt(CASE) ~ OBJECT ~ id ~ templateOpt ^^ {
      case caseOption ~ objectToken ~ name ~ TemplateOpt(templateInheritanceSectionOpt, templateBodyOpt) ⇒
        TmplDef(
          markerTokens = caseOption.toList ++ List(objectToken),
          name = name,
          typeParamClauseOpt = None,
          annotations = Nil,
          accessModifierOpt = None,
          paramClausesOpt = None,
          templateInheritanceSectionOpt = templateInheritanceSectionOpt,
          templateBodyOption = templateBodyOpt)
    }
  }

  def templateParents(isTrait: Boolean): Parser[TemplateParents] = {
    val annotTypeAsType = annotType(isPattern = false) ^^ { x ⇒ Type(typeElementFlatten2(x)) }
    val guardedArgumentExprs = guard(LPAREN) ~> argumentExprs
    annotTypeAsType ~ opt(when(!isTrait, rep(guardedArgumentExprs))) ~ pairRep(WITH, annotTypeAsType) ^^ {
      case type1 ~ guardedArgumentExprss ~ withTypes ⇒ TemplateParents(type1, guardedArgumentExprss getOrElse Nil, withTypes)
    }
  }

  def template(isTrait: Boolean): Parser[Template] = {
    val newlineOptTemplateBody: Parser[TemplateBody] = newlineOpt ~ templateBody(isPre = true) ^^ {
      case newlineOpt ~ templateBody ⇒ templateBody.copy(newlineOpt = newlineOpt)
    }
    val earlyDefsTemplate = newlineOptTemplateBody ~ opt(WITH ~ templateParents(isTrait) ~ templateBodyOpt) ^^ {
      case earlyTemplateBody ~ Some(withToken ~ templateParents ~ templateBodyOpt) ⇒
        Template(Some(EarlyDefs(earlyTemplateBody, Some(withToken))), Some(templateParents), templateBodyOpt)
      case earlyTemplateBody ~ None ⇒
        Template(Some(EarlyDefs(earlyTemplateBody, None)), None, None)
    }

    val justParentsAndBodyTemplate = templateParents(isTrait) ~ templateBodyOpt ^^ {
      case templateParents ~ templateBodyOpt ⇒ Template(None, Some(templateParents), templateBodyOpt)
    }
    earlyDefsTemplate | justParentsAndBodyTemplate
  }

  lazy val templateOpt: Parser[TemplateOpt] = {
    /* Todo: isTrait condition */
    val extendsTemplate = EXTENDS ~ template(isTrait = false) ^^ {
      case extendsToken ~ Template(earlyDefsOpt, templateParentsOpt, templateBodyOpt) ⇒
        TemplateOpt(Some(TemplateInheritanceSection(extendsToken, earlyDefsOpt, templateParentsOpt)), templateBodyOpt)
    }
    val subtypeTemplate = SUBTYPE ~ template(isTrait = true) ^^ {
      case subtypeToken ~ Template(earlyDefsOpt, templateParentsOpt, templateBodyOpt) ⇒
        TemplateOpt(Some(TemplateInheritanceSection(subtypeToken, earlyDefsOpt, templateParentsOpt)), templateBodyOpt)
    }
    val justBody =/* opt(NEWLINE <~ guard(LBRACE)) ~ */templateBodyOpt ^^ { // Omitted the NEWLINEopt because it will be picked up by templateBodyOpt
templateBodyOpt ⇒
      TemplateOpt(None, templateBodyOpt)
    }
    extendsTemplate | subtypeTemplate | justBody
  }

  // -- Templates -----------------------------------------------------------------------
  def templateBody(isPre: Boolean): Parser[TemplateBody] = LBRACE ~ templateStatSeq ~ RBRACE ^^ {
    case lbrace ~ templateStatSeq ~ rbrace ⇒ TemplateBody(None, lbrace, templateStatSeq, rbrace)
  }
  lazy val templateBodyOpt: Parser[Option[TemplateBody]] = opt(opt(NEWLINE whenFollowedBy LBRACE) ~ templateBody(isPre = false)) ^^ { x ⇒
    x map { case newlineOpt ~ templateBody ⇒ templateBody.copy(newlineOpt = newlineOpt) }
  }

  lazy val refinement: Parser[Refinement] = LBRACE ~ refineStatSeq ~ RBRACE ^^ {
    case lbrace ~ refineStatSeq ~ rbrace ⇒ Refinement(lbrace, refineStatSeq, rbrace)
  }

  // -- StatSeqs ------------------------------------------------------------------------

  lazy val packaging = qualId ~ newlineOpt ~ LBRACE ~ topStatSeq ~ RBRACE

  lazy val topStatSeq: Parser[StatSeq] = {
    val packageBlock: Parser[PackageBlock] = PACKAGE ~ packaging ^^ {
      case packageToken ~(name ~ newlineOpt ~ lbrace ~ topStats ~ rbrace) ⇒ PackageBlock(packageToken, name, newlineOpt, lbrace, topStats, rbrace)
    }
    val packageObjectDef: Parser[FullDefOrDcl] = PACKAGE ~ objectDef ^^ {
      case packageToken ~ objectDef ⇒ FullDefOrDcl(annotations = Nil, modifiers = List(SimpleModifier(packageToken)), defOrDcl = objectDef)
    }
    val topStat: Parser[Stat] = packageBlock | packageObjectDef | importClause | topLevelTmplDef
    opt(topStat) ~ pairRep(statSep, opt(topStat)) ^^ {
      case (statOption ~ otherStats) ⇒ StatSeq(None, statOption, otherStats)
    }
  }

  lazy val templateStatSeq: Parser[StatSeq] = {
    val templateStat: Parser[Stat] = importClause | expr0(InTemplate) | nonLocalDefOrDcl
    val plainTemplateStatSeq: Parser[(Option[Stat], List[(Token, Option[Stat])])] = opt(templateStat) ~ pairRep(statSep, opt(templateStat)) ^^ {
      case firstStatOpt ~ otherStats ⇒ (firstStatOpt, otherStats)
    }
    val form1 = expr0(InTemplate) ~ opt((ARROW | statSep) ~ plainTemplateStatSeq) ^^ {
      case firstExpr ~ None ⇒ StatSeq(None, Some(firstExpr), Nil)
      case firstExpr ~ Some(arrowToken ~ x) if arrowToken.getType == ARROW ⇒ { // WTF can't I inline x?
        x match {
          case (firstStatOpt, otherStats) ⇒ StatSeq(Some((firstExpr, arrowToken)), firstStatOpt, otherStats)
        }
      }
      case firstExpr ~ Some(statSep ~ x) ⇒ {
        x match {
          case (firstStatOpt, otherStats) ⇒ StatSeq(None, Some(firstExpr), (statSep, firstStatOpt) :: otherStats)
        }
      }
    }
    val form2 = plainTemplateStatSeq ^^ { case (firstStatOpt, otherStats) ⇒ StatSeq(None, firstStatOpt, otherStats) }
    form1 | form2
  }

  lazy val refineStatSeq: Parser[StatSeq] = {
    val refineDefOrDcl = defOrDcl ^^ { FullDefOrDcl(Nil, Nil, _) }
    opt(refineDefOrDcl) ~ pairRep(statSep, opt(refineDefOrDcl)) ^^ {
      case (statOption ~ otherStats) ⇒ StatSeq(None, statOption, otherStats)
    }
  }

  lazy val localDef: Parser[FullDefOrDcl] = {
    /* TODO: condition -- why tmplDef here when it's covered by defOrDcl already */
    annotations(skipNewLines = true, requireOneArgList = false) ~ localModifiers ~ (defOrDcl | tmplDef) ^^ {
      case annotations ~ localModifiers ~ defOrDcl ⇒ FullDefOrDcl(annotations, localModifiers, defOrDcl)
    }
  }

  val blockStat: Parser[Stat] = {
    val implicitLocalDef: Parser[FullDefOrDcl] = IMPLICIT ~ localDef ^^ {
      case implicitToken ~ localDef ⇒ localDef.copy(modifiers = SimpleModifier(implicitToken) :: localDef.modifiers)
    }
    (importClause whenFollowedBy statSep) |
      expr0(InBlock) |
      implicitLocalDef |
      IMPLICIT ~ implicitClosure(InBlock) ^^ { x ⇒ Expr(exprElementFlatten2(x)) } |
      localDef
  }
  lazy val blockStatSeq = {
    opt(blockStat) ~ pairRep(statSep, opt(blockStat)) ^^ {
      case (blockStatOption ~ otherBlockStats) ⇒
        StatSeq(None, blockStatOption, otherBlockStats)
    }
  }

  lazy val packageBit: Parser[StatSeq] = {
    val packageBit1 = PACKAGE ~ objectDef ~ opt(statSep ~ topStatSeq) ^^ {
      case packageToken ~ objectDef ~ remainderOpt ⇒ {
        val packageStat = FullDefOrDcl(annotations = Nil, modifiers = List(SimpleModifier(packageToken)), defOrDcl = objectDef)
        remainderOpt match {
          case None ⇒ StatSeq(None, Some(packageStat), Nil)
          case Some(statSep ~ StatSeq(_, statOption, otherStats)) ⇒ StatSeq(None, Some(packageStat), (statSep, statOption) :: otherStats)
        }
      }

    }
    val packageBit2 = PACKAGE ~ qualId ~ statSep ~ topStats ^^ {
      case packageToken ~ name ~ statSep ~ StatSeq(_, statOption, otherStats) ⇒
        StatSeq(None, Some(PackageStat(packageToken, name)), (statSep, statOption) :: otherStats)
    }
    val packageBit3 = PACKAGE ~ qualId ~ newlineOpt ~ LBRACE ~ topStatSeq ~ RBRACE ~ topStatSeq ^^ {
      case packageToken ~ name ~ newlineOpt ~ lbrace ~ topStats ~ rbrace ~ StatSeq(_, statOption, otherStats) ⇒ {
        val packageBlock = PackageBlock(packageToken, name, newlineOpt, lbrace, topStats, rbrace)

        // firstStatOpt: Option[Stat], 
        // otherStats: List[(Token, Option[Stat])]

        require(statOption.isEmpty) // can be non-empty, something of an oddity in the grammar as implemented by 2.8, see 
        // https://lampsvn.epfl.ch/trac/scala/ticket/2973

        StatSeq(None, Some(packageBlock), otherStats)
      }
    }
    val packageBit4 = PACKAGE ~ qualId ^^ {
      case packageToken ~ name ⇒ StatSeq(None, Some(PackageStat(packageToken, name)), Nil)
    }
    packageBit1 | packageBit2 | packageBit3 | packageBit4
  }

  lazy val topStats: Parser[StatSeq] = {
    rep(SEMI) ~ (packageBit | topStatSeq) ^^ {
      case Nil ~ topStatSeq ⇒ topStatSeq
      case semis ~ StatSeq(_, statOption, otherStats) ⇒ {
        StatSeq(None, None, (semis.init map { (_, None) }) ::: ((semis.last, statOption) :: otherStats))
      }
    }
  }
  lazy val compilationUnit: Parser[CompilationUnit] = topStats ^^ { CompilationUnit(_) }

  def xmlStartTag(isPattern: Boolean): Parser[XmlStartTag] =
    XML_START_OPEN ~ XML_NAME ~ pairRep(opt(XML_WHITESPACE), xmlAttribute(isPattern)) ~ opt(XML_WHITESPACE) ~ XML_TAG_CLOSE ^^ {
      case startOpen ~ name ~ attributes ~ whitespaceOption ~ tagClose ⇒ XmlStartTag(startOpen, name, attributes, whitespaceOption, tagClose)
    }

  def xmlAttribute(isPattern: Boolean): Parser[XmlAttribute] = {
    XML_NAME ~ opt(XML_WHITESPACE) ~ XML_ATTR_EQ ~ opt(XML_WHITESPACE) ~ (XML_ATTR_VALUE |/ xmlEmbeddedScala(isPattern)) ^^ {
      case name ~ whitespaceOption ~ equals ~ whitespaceOption2 ~ valueOrEmbeddedScala ⇒ XmlAttribute(name, whitespaceOption, equals, whitespaceOption2, valueOrEmbeddedScala)
    }
  }
  def xmlEmptyElement(isPattern: Boolean): Parser[XmlEmptyElement] = {
    XML_START_OPEN ~ XML_NAME ~ pairRep(opt(XML_WHITESPACE), xmlAttribute(isPattern)) ~ opt(XML_WHITESPACE) ~ XML_EMPTY_CLOSE ^^ {
      case startOpen ~ name ~ attributes ~ whitespaceOption ~ emptyClose ⇒ XmlEmptyElement(startOpen, name, attributes, whitespaceOption, emptyClose)
    }
  }
  def xmlEmbeddedScala(isPattern: Boolean): Parser[Expr] = {
    val contents = if (isPattern) (LBRACE ~ patterns(seqOK = true) ~ RBRACE ^^ exprElementFlatten) else blockExpr ^^ exprElementFlatten
    contents ^^ { x ⇒ Expr(exprElementFlatten2(x)) }
  }
  def xmlEndTag: Parser[XmlEndTag] = XML_END_OPEN ~ XML_NAME ~ XML_TAG_CLOSE ^^ {
    case endOpen ~ name ~ tagClose ⇒ XmlEndTag(endOpen, name, tagClose)
  }

  lazy val xmlPCDATA = XML_PCDATA ^^ { XmlPCDATA(_) }
  lazy val xmlComment = XML_COMMENT ^^ { XmlComment(_) }
  lazy val xmlCDATA = XML_CDATA ^^ { XmlCDATA(_) }
  lazy val xmlUnparsed = XML_UNPARSED ^^ { XmlUnparsed(_) }
  lazy val xmlProcessingInstruction = XML_PROCESSING_INSTRUCTION ^^ { XmlProcessingInstruction(_) }

  def xmlElement(isPattern: Boolean): Parser[XmlElement] = {
    lazy val xmlContent: Parser[XmlContents] = xmlElement(isPattern) | xmlPCDATA | xmlComment | xmlCDATA | xmlUnparsed | xmlProcessingInstruction | xmlEmbeddedScala(isPattern)
    val xmlNonEmptyElement: Parser[XmlNonEmptyElement] = xmlStartTag(isPattern) ~ rep(xmlContent) ~ xmlEndTag ^^ {
      case startTag ~ contents ~ endTag ⇒ XmlNonEmptyElement(startTag, contents, endTag)
    }
    xmlNonEmptyElement | xmlEmptyElement(isPattern)
  }
  lazy val xmlExpr = xml(isPattern = false)
  lazy val xmlPattern = xml(isPattern = true)
  def xml(isPattern: Boolean): Parser[XmlExpr] = {
    lazy val xmlContent: Parser[XmlContents] = xmlElement(isPattern) | xmlComment | xmlCDATA | xmlUnparsed | xmlProcessingInstruction
    xmlContent ~ rep(xmlElement(isPattern)) ^^ { case first ~ otherElements ⇒ XmlExpr(first, otherElements) }
  }
}
