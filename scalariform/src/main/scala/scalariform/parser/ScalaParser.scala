package scalariform.parser

import scalariform.lexer.Tokens._
import scalariform.lexer._
import scalariform.utils.Utils._
import scala.collection.mutable.ListBuffer
import PartialFunction.cond

class ScalaParser(tokens: Array[Token]) {

  private val logging: Boolean = false

  import ScalaParser._

  require(!tokens.isEmpty) // at least EOF

  def compilationUnitOrScript(): CompilationUnit = {
    val originalPos = pos
    try {
      compilationUnit()
    } catch {
      case e: ScalaParserException ⇒
        pos = originalPos
        if (logging) println("Rewinding to try alternative: " + currentToken)
        try {
          scriptBody()
        } catch { case e2: ScalaParserException ⇒ throw e }
    }
  }

  def scriptBody(): CompilationUnit = {
    val stmts = templateStatSeq()
    accept(EOF)
    CompilationUnit(stmts)
  }

  private def accept(tokenType: TokenType): Token =
    if (currentTokenType == tokenType)
      nextToken()
    else
      throw new ScalaParserException("Expected token " + tokenType + " but got " + currentToken)

  private def surround[T](open: TokenType, close: TokenType)(f: ⇒ T): (Token, T, Token) = {
    // println("Surround: " + currentToken)
    val openToken = accept(open)
    val body = f
    val closeToken = accept(close)
    (openToken, body, closeToken)
  }

  private def acceptStatSep(): Token =
    if (NEWLINE || NEWLINES || SEMI)
      nextToken()
    else
      throw new ScalaParserException("Expected statement separator but got " + currentToken)

  private def isModifier = currentTokenType match {
    case ABSTRACT | FINAL | SEALED | PRIVATE |
      PROTECTED | OVERRIDE | IMPLICIT | LAZY ⇒ true
    case _ ⇒ false
  }

  private def isLocalModifier: Boolean = currentTokenType match {
    case ABSTRACT | FINAL | SEALED | IMPLICIT | LAZY ⇒ true
    case _ ⇒ false
  }

  private def isDefIntro: Boolean = currentTokenType match {
    case VAL | VAR | DEF | TYPE | OBJECT | CLASS | TRAIT ⇒ true
    case CASE if caseObject ⇒ true
    case CASE if caseClass ⇒ true
    case _ ⇒ false
  }

  private def isDclIntro: Boolean = currentTokenType match {
    case VAL | VAR | DEF | TYPE ⇒ true
    case _ ⇒ false
  }

  private def isNumericLit: Boolean = currentTokenType match {
    case INTEGER_LITERAL | FLOATING_POINT_LITERAL ⇒ true
    case _ ⇒ false
  }

  private def isUnaryOp: Boolean = currentTokenType match {
    case MINUS | PLUS | TILDE | EXCLAMATION ⇒ true
    case _ ⇒ false
  }

  private def isIdent: Boolean = isIdent(currentTokenType)

  private def isIdent(tokenType: TokenType) = tokenType match {
    case VARID | OTHERID | PLUS | MINUS | STAR | PIPE | TILDE | EXCLAMATION ⇒ true
    case _ ⇒ false
  }

  private def isExprIntroToken(tokenType: TokenType): Boolean = tokenType match {
    case CHARACTER_LITERAL | INTEGER_LITERAL | FLOATING_POINT_LITERAL |
      STRING_LITERAL | SYMBOL_LITERAL | TRUE | FALSE | NULL |
      THIS | SUPER | IF | FOR | NEW | USCORE | TRY | WHILE |
      DO | RETURN | THROW | LPAREN | LBRACE ⇒ true
    case XML_START_OPEN | XML_UNPARSED | XML_COMMENT | XML_CDATA | XML_PROCESSING_INSTRUCTION ⇒ true
    case _ if isIdent(tokenType) ⇒ true
    case _ ⇒ false
  }

  private def isExprIntro: Boolean = isExprIntroToken(currentTokenType)

  private def isTypeIntroToken(tokenType: TokenType): Boolean = tokenType match {
    case THIS | SUPER | USCORE | LPAREN | AT ⇒ true
    case _ if isIdent(tokenType) ⇒ true
    case _ ⇒ false
  }

  private def isTypeIntro: Boolean = isTypeIntroToken(currentTokenType)

  private def isStatSep(tokenType: TokenType) =
    tokenType == NEWLINE || tokenType == NEWLINES || tokenType == SEMI

  private def isStatSep: Boolean = isStatSep(currentTokenType)

  private def commaSeparated[T](runParser: ⇒ T): (T, List[(Token, T)]) = {
    val ts = new ListBuffer[(Token, T)]
    val first = runParser
    while (COMMA) {
      val commaToken = nextToken()
      val nextPart = runParser
      ts += ((commaToken, nextPart))
    }
    (first, ts.toList)
  }

  private def ident(): Token =
    if (isIdent)
      nextToken()
    else
      throw new ScalaParserException("Expected identifier, but got " + currentToken)

  private def selector(): Token = ident()

  private def path(thisOK: Boolean, typeOK: Boolean): List[Token] = {
    val tokens = ListBuffer[Token]()
    if (THIS) {
      tokens += nextToken()
      if (!thisOK || DOT) {
        tokens += accept(DOT)
        tokens ++= selectors(typeOK)
      }
    } else if (SUPER) {
      tokens += nextToken()
      tokens ++= mixinQualifierOpt()
      tokens += accept(DOT)
      tokens += selector()
      if (DOT) {
        tokens += nextToken()
        tokens ++= selectors(typeOK)
      }
    } else {
      tokens += ident()
      if (DOT) {
        tokens += nextToken()
        if (THIS) {
          tokens += nextToken()
          if (!thisOK || DOT) {
            tokens += accept(DOT)
            tokens ++= selectors(typeOK)
          }
        } else if (SUPER) {
          tokens += nextToken()
          tokens ++= mixinQualifierOpt()
          tokens += accept(DOT)
          tokens += selector()
          if (DOT) {
            tokens += nextToken()
            tokens ++= selectors(typeOK)
          }
        } else
          tokens ++= selectors(typeOK)
      }
    }
    tokens.toList
  }

  private def selectors(typeOK: Boolean): List[Token] = {
    val tokens = ListBuffer[Token]()
    if (typeOK && TYPE)
      tokens += nextToken()
    else {
      tokens += selector()
      if (DOT) {
        tokens += nextToken()
        tokens ++= selectors(typeOK)
      }
    }
    tokens.toList
  }

  private def mixinQualifierOpt(): List[Token] =
    if (LBRACKET) {
      val lbracket = nextToken()
      val id = ident()
      val rbracket = accept(RBRACKET)
      List(lbracket, id, rbracket)
    } else
      Nil

  private def stableId(): List[Token] = path(thisOK = false, typeOK = false)

  private def qualId(): List[Token] = {
    val tokens = ListBuffer[Token]()
    tokens += ident()
    if (DOT) {
      tokens += nextToken()
      tokens ++= selectors(typeOK = false)
    }
    tokens.toList
  }

  private def literal(): Token =
    if (CHARACTER_LITERAL || INTEGER_LITERAL || FLOATING_POINT_LITERAL || STRING_LITERAL || SYMBOL_LITERAL || TRUE || FALSE || NULL)
      nextToken()
    else
      throw new ScalaParserException("illegal literal: " + currentToken)

  private def newLineOpt(): Option[Token] = if (NEWLINE) Some(nextToken()) else None

  private def newLinesOpt() = if (NEWLINE || NEWLINES) Some(nextToken()) else None

  private def newLineOptWhenFollowedBy(tokenType: TokenType) =
    if (NEWLINE && lookahead(1) == tokenType)
      newLineOpt()
    else
      None

  private def newLineOptWhenFollowing(pred: TokenType ⇒ Boolean) =
    if (NEWLINE && pred(lookahead(1)))
      newLineOpt()
    else
      None

  private def typedOpt(): Option[(Token, Type)] =
    if (COLON) {
      val colonToken = nextToken()
      val typ_ = typ()
      Some(colonToken, typ_)
    } else
      None

  private def types(isPattern: Boolean, isTypeApply: Boolean, isFuncArg: Boolean): List[TypeElement] = {
    typeElementFlatten3(commaSeparated(argType(isPattern, isTypeApply, isFuncArg)))
  }

  def typ(): Type = log("typ") {
    typ(isPattern = false)
  }

  private def typ(isPattern: Boolean): Type = {
    // println("typ(isPattern = " + isPattern + ")  " + currentToken)
    val others2 = if (LPAREN) {
      val lparen = nextToken()
      if (RPAREN) {
        val rparen = nextToken()
        val arrowToken = accept(ARROW)
        val typ_ = typ(isPattern)
        typeElementFlatten3(lparen, rparen, arrowToken, typ_)
      } else {
        val types_ = types(isPattern, isTypeApply = false, isFuncArg = true)
        val rparen = accept(RPAREN)
        val others = if (ARROW) {
          val arrowToken = nextToken()
          val typ_ = typ(isPattern)
          typeElementFlatten3(arrowToken, typ_)
        } else {
          val simpleTypeRest_ = simpleTypeRest(isPattern)
          val annotTypeRest_ = annotTypeRest()
          val compoundTypeRest_ = compoundTypeRest(isPattern)
          val infixTypeRest_ = infixTypeRest(isPattern)
          typeElementFlatten3(simpleTypeRest_, annotTypeRest_, compoundTypeRest_, infixTypeRest_)
        }
        typeElementFlatten3(lparen, types_, rparen, others)
      }
    } else {
      infixType(isPattern)
    }
    val others3 = if (ARROW) {
      val arrowToken = nextToken()
      val typ_ = typ(isPattern)
      typeElementFlatten3(arrowToken, typ_)
    } else if (FORSOME) {
      val forSomeToken = nextToken()
      val refinement_ = refinement()
      typeElementFlatten3(forSomeToken, refinement_)
    } else
      Nil
    Type(typeElementFlatten3(others2, others3))
  }

  private def infixType(isPattern: Boolean): List[TypeElement] = {
    val compoundType_ = compoundType(isPattern)
    val infixTypeRest_ = infixTypeRest(isPattern)
    typeElementFlatten3(compoundType_, infixTypeRest_)
  }

  private def typeOrInfixType(location: Location): TypeExprElement =
    TypeExprElement(
      if (location == Local)
        typ().contents
      else
        infixType(isPattern = false))

  private def infixTypeRest(isPattern: Boolean): List[TypeElement] = {
    if (isIdent && !STAR) {
      val identToken = currentToken
      val id = InfixTypeConstructor(ident())
      val newLineOpt = newLineOptWhenFollowing(isTypeIntroToken)
      if (isLeftAssoc(identToken)) {
        val compoundType_ = compoundType(isPattern)
        val infixTypeRest_ = infixTypeRest(isPattern)
        typeElementFlatten3(id, newLineOpt, compoundType_, infixTypeRest_)
      } else {
        val infixType_ = infixType(isPattern)
        typeElementFlatten3(id, newLineOpt, infixType_)
      }
    } else
      Nil
  }

  private def compoundType(isPattern: Boolean): List[TypeElement] = {
    val annotTypeOpt = if (LBRACE) None else Some(annotType(isPattern))
    val rest = compoundTypeRest(isPattern)
    typeElementFlatten3(annotTypeOpt, rest)
  }

  private def compoundTypeRest(isPattern: Boolean): List[TypeElement] = {
    val withTypes = ListBuffer[(Token, List[TypeElement])]()
    while (WITH) {
      val withToken = nextToken()
      val annotType_ = annotType(isPattern)
      withTypes += ((withToken, annotType_))
    }
    val newLineOpt = newLineOptWhenFollowedBy(LBRACE)
    val refinementOpt = if (LBRACE) Some(refinement()) else None
    typeElementFlatten3(withTypes.toList, newLineOpt, refinementOpt)
  }

  private def annotType(isPattern: Boolean): List[TypeElement] = {
    val simpleType_ = simpleType(isPattern)
    val annotTypeRest_ = annotTypeRest()
    typeElementFlatten3(simpleType_, annotTypeRest_)
  }

  private def annotTypeRest(): List[TypeElement] =
    typeElementFlatten3(annotations(skipNewLines = false, requireOneArgList = false))

  private def simpleType(isPattern: Boolean): List[TypeElement] = {
    val firstPart = if (LPAREN) {
      val lparen = nextToken()
      val types_ = types(isPattern, isTypeApply = false, isFuncArg = false)
      val rparen = accept(RPAREN)
      typeElementFlatten3(lparen, types_, rparen)
    } else if (USCORE) {
      val uscore = nextToken()
      val wildcardType_ = wildcardType()
      typeElementFlatten3(uscore, wildcardType_)
    } else {
      typeElementFlatten3(path(thisOK = false, typeOK = true))
    }
    val simpleTypeRest_ = simpleTypeRest(isPattern)
    typeElementFlatten3(firstPart, simpleTypeRest_)
  }

  private def simpleTypeRest(isPattern: Boolean): List[TypeElement] = {
    if (HASH) {
      val hashToken = nextToken()
      val id = ident()
      val simpleTypeRest_ = simpleTypeRest(isPattern)
      typeElementFlatten3(hashToken, id, simpleTypeRest_)
    } else if (LBRACKET) {
      val typeArgs_ = typeArgs(isPattern, isTypeApply = false)
      val simpleTypeRest_ = simpleTypeRest(isPattern)
      typeElementFlatten3(typeArgs_, simpleTypeRest_)
    } else
      Nil
  }

  private def wildcardType(): List[TypeElement] = {
    typeBounds()
  }

  private def typeArgs(isPattern: Boolean, isTypeApply: Boolean): List[TypeElement] = {
    val lbracket = accept(LBRACKET)
    val types_ = types(isPattern, isTypeApply, isFuncArg = false)
    val rbracket = accept(RBRACKET)
    typeElementFlatten3(lbracket, types_, rbracket)
  }

  private def argType(isPattern: Boolean, isTypeApply: Boolean, isFuncArg: Boolean): List[TypeElement] =
    log("argType(isPattern = " + isPattern + ", isFuncArg = " + isFuncArg + ")") {
      // println("argType: isPattern = " + isPattern + ", " + currentToken )
      if (isPattern) {
        if (USCORE) {
          val uscore = nextToken()
          val wildcardTypeOpt = if (SUBTYPE || SUPERTYPE) Some(wildcardType()) else None
          typeElementFlatten3(uscore, wildcardTypeOpt)
        } else if (isIdent && isVariableName(currentToken.getText)) {
          typeElementFlatten3(ident())
        } else
          List(typ(isPattern = true))
      } else if (isFuncArg) {
        if (ARROW) {
          val arrowToken = CallByNameTypeElement(nextToken())
          val typ_ = typ()
          typeElementFlatten3(arrowToken, typ_)
        } else {
          val typ_ = typ()
          val starOpt = if (STAR) Some(VarargsTypeElement(nextToken())) else None
          typeElementFlatten3(typ_, starOpt)
        }
      } else {
        List(typ())
      }
    }

  private def equalsExpr() = {
    val equalsToken = accept(EQUALS)
    val expr_ = expr()
    (equalsToken, expr_)
  }

  private def condExpr(): CondExpr = {
    if (LPAREN) {
      val lparen = nextToken()
      val expr_ = expr()
      val rparen = accept(RPAREN)
      CondExpr(lparen, expr_, rparen)
    } else {
      val lparen = accept(LPAREN)
      // Seriously, WTF?
      throw new ScalaParserException("Straggling lparen thing")
    }
  }

  private def statement(location: Location): Expr = expr(location)

  def expr(): Expr = { expr(Local) }

  private def expr(location: Location): Expr = {
    expr0(location)
  }

  private def expr0(location: Location): Expr = {
    makeExpr(currentTokenType match {
      case IF ⇒
        val ifToken = nextToken()
        val condExpr_ = condExpr()
        val newLinesOpt_ = newLinesOpt()
        val body = expr()
        // nsc has merged the SEMI into the ELSE by this point:
        val semiOpt = if (SEMI && lookahead(1) == ELSE) Some(nextToken()) else None
        val elseClauseOption = if (ELSE) {
          val elseToken = nextToken()
          val expr_ = expr()
          Some(ElseClause(semiOpt, elseToken, expr_))
        } else
          None
        List(IfExpr(ifToken, condExpr_, newLinesOpt_, body, elseClauseOption))

      case TRY ⇒
        val tryToken = nextToken()
        val body: Expr = if (LBRACE) {
          val (lbrace, block_, rbrace) = surround(LBRACE, RBRACE)(block())
          makeExpr(BlockExpr(lbrace, Right(block_), rbrace))
        } else if (LPAREN)
          makeExpr(surround(LPAREN, RPAREN)(expr()))
        else
          expr()
        val catchClauseOption = if (CATCH) {
          val catchToken = nextToken()
          val (lbrace, caseClauses_, rbrace) = surround(LBRACE, RBRACE)(caseClauses())
          Some(catchToken, BlockExpr(lbrace, Left(caseClauses_), rbrace))
        } else
          None
        val finallyClauseOption = if (FINALLY) {
          val finallyToken = nextToken()
          val finallyExpr = expr()
          Some(finallyToken, finallyExpr)
        } else
          None
        List(TryExpr(tryToken, body, catchClauseOption, finallyClauseOption))

      case WHILE ⇒
        val whileToken = nextToken()
        val condExpr_ = condExpr()
        val newLinesOpt_ = newLinesOpt()
        val body = expr()
        List(WhileExpr(whileToken, condExpr_, newLinesOpt_, body))

      case DO ⇒
        val doToken = nextToken()
        val body = expr()
        val statSepOpt = if (isStatSep) Some(nextToken()) else None
        val whileToken = accept(WHILE)
        val condExpr_ = condExpr()
        List(DoExpr(doToken, body, statSepOpt, whileToken, condExpr_))

      case FOR ⇒
        val forToken = nextToken()
        val (open, close) = if (LBRACE) (LBRACE, RBRACE) else (LPAREN, RPAREN)
        val (lParenOrBrace, enumerators_, rParenOrBrace) = surround(open, close)(enumerators())
        val newlinesOption = newLinesOpt()
        val (yieldOption, body) = if (YIELD) {
          val yieldToken = nextToken()
          (Some(yieldToken), expr())
        } else
          (None, expr())
        List(ForExpr(forToken, lParenOrBrace, enumerators_, rParenOrBrace, newlinesOption, yieldOption, body))

      case RETURN ⇒
        val returnToken = nextToken()
        val returnExpr = if (isExprIntro) Some(expr()) else None
        exprElementFlatten2(returnToken, returnExpr) // TODO: <-- use a different type?

      case THROW ⇒
        val throwToken = nextToken()
        val throwExpr = expr()
        exprElementFlatten2(throwToken, throwExpr)

      case IMPLICIT ⇒
        val implicitToken = nextToken()
        List(implicitClosure(location, implicitToken))

      case _ ⇒

        val postfixExpr_ = postfixExpr()
        val equalsColonOrMatchSuffix = if (EQUALS) {
          exprElementFlatten2(optional { /* TODO: case Ident(_) | Select(_, _) | Apply(_, _) => */
            val equalsToken = nextToken()
            val equalsExpr = expr()
            (equalsToken, equalsExpr)
          })
        } else if (COLON) {
          val colonToken = nextToken()
          if (USCORE) {
            val uscore = nextToken()
            val star = accept(STAR)
            exprElementFlatten2(colonToken, uscore, star)
          } else if (AT) {
            exprElementFlatten2(colonToken, annotations(skipNewLines = false, requireOneArgList = false))
          } else {
            val type_ = typeOrInfixType(location)
            exprElementFlatten2(colonToken, type_)
          }
        } else if (MATCH) {
          val matchToken = nextToken()
          val (lbrace, caseClauses_, rbrace) = surround(LBRACE, RBRACE)(caseClauses())
          val blockExpr_ = BlockExpr(lbrace, Left(caseClauses_), rbrace)
          exprElementFlatten2(matchToken, blockExpr_)
        } else
          Nil

        if (logging)
          println("in expr0, postfixExpr = " + postfixExpr_)

        val intermediateResult = exprElementFlatten2(postfixExpr_, equalsColonOrMatchSuffix)
        val lhsIsTypedParamList = cond(postfixExpr_) { case List(ParenExpr(_, _, _)) ⇒ true } // TODO: is this check sufficient?
        if (ARROW && (location != InTemplate || lhsIsTypedParamList)) {
          val anonFuncOpt: Option[List[ExprElement]] = optional {
            val arrowToken = nextToken()
            val postArrow = if (location != InBlock) expr() else block()
            exprElementFlatten2(AnonymousFunction(intermediateResult, arrowToken, List(postArrow)))
          }
          anonFuncOpt match {
            case None ⇒ intermediateResult
            case Some(stuff) ⇒ stuff
          } // erk, why can't I use getOrElse here!?
        } else
          intermediateResult
    })
  }

  private def implicitClosure(location: Location, implicitToken: Token): AnonymousFunction = {
    val id = ident()
    val colonTypeOpt = if (COLON) {
      val colonToken = nextToken()
      val type_ = typeOrInfixType(location)
      Some(colonToken, type_)
    } else
      None
    val arrowToken = accept(ARROW)
    val body = exprElementFlatten2(if (location != InBlock) expr() else block())
    AnonymousFunction(exprElementFlatten2(implicitToken, id, colonTypeOpt), arrowToken, body)
  }

  private def postfixExpr(): List[ExprElement] = {
    val prefixExpr_ = prefixExpr()
    val postfixParts = ListBuffer[List[ExprElement]]()
    while (isIdent) {
      val id = ident()
      val newLineOpt = newLineOptWhenFollowing(isExprIntroToken)
      val postfixPart = if (isExprIntro)
        exprElementFlatten2(InfixExprElement(id), newLineOpt, prefixExpr())
      else
        List(PostfixExprElement(id))
      postfixParts += postfixPart
    }
    exprElementFlatten2(prefixExpr_, postfixParts.toList)
  }

  private def prefixExpr(): List[ExprElement] = {
    if (isUnaryOp) {
      val isMinus = MINUS
      val unaryId = PrefixExprElement(ident())
      if (isMinus && isNumericLit) {
        val literal_ = literal()
        val simpleExprRest_ = simpleExprRest(true)
        exprElementFlatten2(unaryId, literal_, simpleExprRest_)
      } else
        exprElementFlatten2(unaryId, simpleExpr())
    } else
      simpleExpr()
  }

  private def simpleExpr(): List[ExprElement] = {
    var canApply = true
    val firstPart = currentTokenType match {
      case CHARACTER_LITERAL | INTEGER_LITERAL | FLOATING_POINT_LITERAL | STRING_LITERAL | SYMBOL_LITERAL | TRUE | FALSE | NULL ⇒
        exprElementFlatten2(literal())
      case XML_START_OPEN | XML_COMMENT | XML_CDATA | XML_UNPARSED | XML_PROCESSING_INSTRUCTION ⇒
        exprElementFlatten2(xmlLiteral())
      case VARID | OTHERID | PLUS | MINUS | STAR | PIPE | TILDE | EXCLAMATION | THIS | SUPER ⇒
        exprElementFlatten2(path(thisOK = true, typeOK = false))
      case USCORE ⇒
        exprElementFlatten2(nextToken())
      case LPAREN ⇒
        val lparen = nextToken
        val parenBody = if (RPAREN)
          None
        else
          Some(commaSeparated(expr))
        val rparen = accept(RPAREN)
        exprElementFlatten2(ParenExpr(lparen, exprElementFlatten2(parenBody), rparen))
      case LBRACE ⇒
        canApply = false
        exprElementFlatten2(blockExpr())
      case NEW ⇒
        canApply = false
        val newToken = nextToken()
        val template_ = template(isTrait = false)
        exprElementFlatten2(newToken, template_)
      case _ ⇒
        throw new ScalaParserException("illegal start of simple expression: " + currentToken)
    }
    val remainder = simpleExprRest(canApply)
    exprElementFlatten2(firstPart, remainder)
  }

  private def simpleExprRest(canApply: Boolean): List[ExprElement] = {
    val newLineOpt = if (canApply) newLineOptWhenFollowedBy(LBRACE) else None
    currentTokenType match {
      case DOT ⇒
        val dot = nextToken()
        val selector_ = selector()
        val simpleExprRest_ = simpleExprRest(canApply = true)
        exprElementFlatten2(newLineOpt, (dot, selector_, simpleExprRest_))
      case LBRACKET ⇒
        val identifierCond = true /* TODO */ /*             case Ident(_) | Select(_, _) => */
        if (identifierCond) {
          val typeArgs_ = TypeExprElement(typeArgs(isPattern = false, isTypeApply = true))
          val simpleExprRest_ = simpleExprRest(canApply = true)
          exprElementFlatten2(newLineOpt, typeArgs_, simpleExprRest_)
        } else
          exprElementFlatten2(newLineOpt)
      case LPAREN | LBRACE if canApply ⇒
        val argumentExprs_ = argumentExprs()
        val simpleExprRest_ = simpleExprRest(canApply = true)
        exprElementFlatten2(newLineOpt, argumentExprs_, simpleExprRest_)
      case USCORE ⇒
        exprElementFlatten2(newLineOpt, PostfixExprElement(nextToken()))
      case _ ⇒
        exprElementFlatten2(newLineOpt)
    }
  }

  private def argumentExprs(): ArgumentExprs = {
    // println("argumentExprs(): " + currentToken)
    def args() = commaSeparated(expr)
    if (LBRACE) {
      BlockArgumentExprs(exprElementFlatten2(blockExpr()))
    } else {
      val (lparen, body, rparen) = surround(LPAREN, RPAREN) {
        if (RPAREN)
          Nil
        else
          exprElementFlatten2(args())
      }
      ParenArgumentExprs(lparen, body, rparen)
    }
  }

  private def blockExpr(): BlockExpr = {
    val lbrace = accept(LBRACE)
    val body = if (justCase) Left(caseClauses())
    else Right(block())
    val rbrace = accept(RBRACE)
    BlockExpr(lbrace, body, rbrace)
  }

  private def block(): StatSeq = {
    blockStatSeq()
  }

  private def caseClauses(): CaseClauses = {
    var caseClauses_ = Vector[CaseClause]()
    do {
      caseClauses_ = caseClauses_ :+ caseClause()
    } while (justCase)
    CaseClauses(caseClauses_.toList)
  }

  private def caseClause(): CaseClause = {
    val caseToken = accept(CASE)
    val pattern_ = pattern()
    val guardOption = guard()
    val (arrow, blockStatSeq_) = caseBlock()
    CaseClause(caseToken, pattern_, guardOption, arrow, blockStatSeq_)
  }

  private def caseBlock(): (Token, StatSeq) = {
    val arrowToken = accept(ARROW)
    val blockStatSeq_ = block()
    (arrowToken, blockStatSeq_)
  }

  private def guard(): Option[Guard] = {
    if (IF) {
      val ifToken = nextToken()
      val postfixExpr_ = postfixExpr()
      Some(Guard(ifToken, Expr(postfixExpr_)))
    } else
      None
  }

  private def enumerators(): Enumerators = {
    val newStyle = !VAL
    val initialGenerator = generator(eqOK = false)
    val otherEnumerators = ListBuffer[(Token, Enumerator)]()
    while (isStatSep) {
      val statSep = nextToken()
      val enumerator = if (newStyle) {
        if (IF) guard().get
        else generator(eqOK = true)
      } else {
        if (VAL) generator(eqOK = true)
        else expr()
      }
      otherEnumerators += ((statSep, enumerator))
    }
    Enumerators(initialGenerator, otherEnumerators.toList)
  }

  private def generator(eqOK: Boolean): Generator = {
    val valOption = if (VAL) Some(nextToken()) else None
    val pattern_ = pattern1(seqOK = false)
    val equalsOrArrowToken = if (EQUALS && eqOK) nextToken() else accept(LARROW)
    val expr_ = expr()
    val guards = ListBuffer[Guard]()
    while (IF) guards += guard().get
    Generator(valOption, pattern_, equalsOrArrowToken, expr_, guards.toList)
  }

  private def patterns(seqOK: Boolean): List[ExprElement] = {
    exprElementFlatten2(commaSeparated(pattern(seqOK)))
  }

  private def pattern(seqOK: Boolean): Expr = {
    val firstPattern = pattern1(seqOK)
    val pipeOtherPatterns = ListBuffer[(InfixExprElement, Expr)]()
    if (PIPE) {
      while (PIPE) {
        val pipeElement = InfixExprElement(nextToken())
        val otherPattern = pattern1(seqOK)
        pipeOtherPatterns += ((pipeElement, otherPattern))
      }
    }
    makeExpr(firstPattern, pipeOtherPatterns.toList)
  }

  private def pattern(): Expr = {
    pattern(seqOK = false)
  }

  private def pattern1(seqOK: Boolean): Expr = {
    val firstPattern = pattern2(seqOK)
    val colonTypeOpt = if (COLON) { // TODO: case Ident(name) if (treeInfo.isVarPattern(p) && in.token == COLON)
      val colonToken = nextToken()
      val compoundType_ = Some(TypeExprElement(compoundType(isPattern = true)))
      Some(colonToken, compoundType_)
    } else
      None
    makeExpr(firstPattern, colonTypeOpt)
  }

  private def pattern2(seqOK: Boolean): Expr = {
    val firstPattern = pattern3(seqOK)
    val atOtherOpt = if (AT) {
      // val identWildcard = false
      // val varPattern = false
      //        case Ident(name) =>       if (name == nme.WILDCARD) {
      // if (identWildcard) {
      //   nextToken()
      //   pattern3(seqOK)
      // } else if (varPattern) { // (treeInfo.isVarPattern(p)) {
      //   nextToken()
      //   pattern3(seqOK)
      // }

      optional {
        val atToken = nextToken()
        val otherPattern = pattern3(seqOK)
        (atToken, otherPattern)
      }
    } else
      None
    makeExpr(firstPattern, atOtherOpt)
  }

  private def pattern3(seqOK: Boolean): List[ExprElement] = {

    val simplePattern1 = simplePattern(seqOK)
    if (seqOK && STAR) {
      val starToken = nextToken()
      exprElementFlatten2(simplePattern1, starToken)
    } else {
      val idAndPatterns = ListBuffer[(Token, List[ExprElement])]()
      while (isIdent && !PIPE) {
        val id = ident()
        val otherSimplePattern = simplePattern(seqOK)
        idAndPatterns += ((id, otherSimplePattern))
      }
      exprElementFlatten2(simplePattern1, idAndPatterns.toList)
    }
  }

  private def simplePattern(seqOK: Boolean): List[ExprElement] = {
    // println("simplePattern: " + currentToken)
    currentTokenType match {
      case VARID | OTHERID | PLUS | MINUS | STAR | PIPE | TILDE | EXCLAMATION | THIS ⇒
        val nameIsMinus: Boolean = MINUS // TODO  case Ident(name) if name == nme.MINUS =>
        val id = stableId()
        val literalOpt = currentTokenType match {
          case INTEGER_LITERAL | FLOATING_POINT_LITERAL ⇒
            if (nameIsMinus) Some(literal())
            else None
          case _ ⇒ None // TODO: condOpt
        }
        val argumentPatternsOpt = if (LPAREN) Some(argumentPatterns()) else None
        exprElementFlatten2(id, literalOpt, argumentPatternsOpt)
      case USCORE ⇒
        exprElementFlatten2(nextToken())
      case CHARACTER_LITERAL | INTEGER_LITERAL | FLOATING_POINT_LITERAL | STRING_LITERAL | SYMBOL_LITERAL | TRUE | FALSE | NULL ⇒
        exprElementFlatten2(literal())
      case LPAREN ⇒
        val lparen = nextToken()
        val patterns_ = if (RPAREN)
          Nil
        else
          patterns(seqOK = false)
        val rparen = accept(RPAREN)
        exprElementFlatten2(lparen, patterns_, rparen)
      case XML_START_OPEN | XML_COMMENT | XML_CDATA | XML_UNPARSED | XML_PROCESSING_INSTRUCTION ⇒
        exprElementFlatten2(xmlLiteralPattern())
      case _ ⇒
        throw new ScalaParserException("illegal start of simple pattern: " + currentToken)
    }
  }

  private def argumentPatterns(): List[ExprElement] = {
    val lparen = accept(LPAREN)
    val patterns_ = if (RPAREN)
      Nil
    else
      patterns(seqOK = true)
    val rparen = accept(RPAREN)
    exprElementFlatten2(lparen, patterns_, rparen)
  }

  private def accessQualifierOpt(): Option[AccessQualifier] =
    if (LBRACKET) {
      val lbracket = nextToken()
      val thisOrId = if (THIS)
        nextToken()
      else
        ident()
      val rbracket = accept(RBRACKET)
      Some(AccessQualifier(lbracket, thisOrId, rbracket))
    } else
      None

  private def accessModifierOpt(): Option[AccessModifier] = {
    currentTokenType match {
      case PRIVATE | PROTECTED ⇒
        val privateOrProtected = nextToken()
        val accessQualifierOpt_ = accessQualifierOpt()
        Some(AccessModifier(privateOrProtected, accessQualifierOpt_))
      case _ ⇒
        None
    }
  }

  private def modifiers(): List[Modifier] = {
    val modifiers = ListBuffer[Modifier]()
    def loop() {
      currentTokenType match {
        case PRIVATE | PROTECTED ⇒
          val privateOrProtected = nextToken()
          val accessQualifierOpt_ = accessQualifierOpt()
          modifiers += AccessModifier(privateOrProtected, accessQualifierOpt_)
          loop()
        case ABSTRACT | FINAL | SEALED | OVERRIDE | IMPLICIT | LAZY ⇒
          modifiers += SimpleModifier(nextToken())
          loop()
        case NEWLINE ⇒
          modifiers += SimpleModifier(nextToken()) // Investigate
          loop()
        case _ ⇒
      }
    }
    loop()
    modifiers.toList
  }

  private def localModifiers(): List[Modifier] = {
    val modifiers = ListBuffer[Modifier]()
    def loop() {
      currentTokenType match {
        case ABSTRACT | FINAL | SEALED | IMPLICIT | LAZY ⇒
          modifiers += SimpleModifier(nextToken())
          loop()
        case _ ⇒
      }
    }
    loop()
    modifiers.toList
  }

  private def annotations(skipNewLines: Boolean, requireOneArgList: Boolean): List[Annotation] = {
    val annotations = ListBuffer[Annotation]()
    while (AT) {
      val atToken = nextToken()
      val (annotationType, argumentExprss) = annotationExpr(requireOneArgList)
      val newLineOpt_ = if (skipNewLines) newLineOpt() else None
      annotations += Annotation(atToken, annotationType, argumentExprss, newLineOpt_)
    }
    annotations.toList
  }

  private def annotationExpr(requireOneArgList: Boolean): (Type, List[ArgumentExprs]) = {
    val annotationType = Type(simpleType(isPattern = false))
    val argumentExprss = ListBuffer[ArgumentExprs]()
    if (requireOneArgList)
      argumentExprss += argumentExprs()
    else if (LPAREN)
      do {
        argumentExprss += argumentExprs()
      } while (LPAREN)
    (annotationType, argumentExprss.toList)
  }

  private def paramClauses(): ParamClauses = {
    var implicitmod = false

    def param(): Param = {
      val annotations_ = annotations(skipNewLines = false, requireOneArgList = false)
      val ownerIsTypeName = true // TODO: if (owner.isTypeName)
      val modifiers_ = ListBuffer[Modifier]()
      val valOrVarOpt = if (ownerIsTypeName) {
        modifiers_ ++= modifiers()
        if (VAL) // TODO: Check -- two vals
          Some(nextToken())
        else if (VAR)
          Some(nextToken())
        else
          None
      } else
        None
      val id = ident()
      val colonToken = accept(COLON)
      val paramType_ = paramType()
      val paramTypeOpt = Some(colonToken, paramType_)
      val defaultValueOpt = if (EQUALS) {
        val equalsToken = nextToken()
        val expr_ = expr()
        Some(equalsToken, expr_)
      } else
        None
      Param(annotations_, modifiers_.toList, valOrVarOpt, id, paramTypeOpt, defaultValueOpt)
    }

    // Differs from nsc in that we've pulled in lparen/rparen
    def paramClause(): ParamClause = {
      val lparen = accept(LPAREN)
      if (RPAREN) {
        val rparen = accept(RPAREN)
        ParamClause(lparen, None, None, Nil, rparen)
      } else {
        val implicitOption = if (IMPLICIT) {
          val implicitToken = nextToken()
          implicitmod = true
          Some(implicitToken)
        } else
          None
        val param_ = param()
        val otherParams = ListBuffer[(Token, Param)]()
        while (COMMA) {
          val comma = nextToken()
          val otherParam = param()
          otherParams += ((comma, otherParam))
        }
        val rparen = accept(RPAREN)
        ParamClause(lparen, implicitOption, Some(param_), otherParams.toList, rparen)
      }
    }

    val newLineOpt = newLineOptWhenFollowedBy(LPAREN)

    val paramClausesAndNewline = ListBuffer[(ParamClause, Option[Token])]()
    while (!implicitmod && LPAREN) {
      val paramClause_ = paramClause()
      val newLineOpt2 = newLineOptWhenFollowedBy(LPAREN)
      paramClausesAndNewline += ((paramClause_, newLineOpt2))
    }
    ParamClauses(newLineOpt, paramClausesAndNewline.toList)
  }

  private def paramType(): Type = log("paramType") {
    val typeElements = ListBuffer[TypeElementFlattenable]()
    if (ARROW) {
      typeElements += nextToken()
      typeElements += typ()
    } else {
      typeElements += typ()
      if (STAR)
        typeElements += VarargsTypeElement(nextToken())
    }
    Type(typeElementFlatten3(typeElements.toList))
  }

  private def typeParamClauseOpt(): Option[TypeParamClause] = {
    def typeParam(): List[TypeElement] = {
      val typeElements = ListBuffer[TypeElementFlattenable]()
      if (isIdent) { // TODO: condition 
        if (PLUS)
          typeElements += VarianceTypeElement(nextToken())
        else if (MINUS)
          typeElements += VarianceTypeElement(nextToken())
      }
      if (USCORE)
        typeElements += nextToken()
      else
        typeElements += ident()
      typeElements += typeParamClauseOpt()
      typeElements += typeBounds()
      while (VIEWBOUND) {
        typeElements += nextToken()
        typeElements += typ()
      }
      while (COLON) {
        typeElements += nextToken()
        typeElements += typ()
      }
      typeElementFlatten3(typeElements.toList)
    }

    val newLineOpt = newLineOptWhenFollowedBy(LBRACKET)
    if (LBRACKET) {
      val lbracket = nextToken()
      val annotations_ = annotations(skipNewLines = true, requireOneArgList = false)
      val typeParam1 = typeParam()
      val commaTypeParams = ListBuffer[TypeElementFlattenable]()
      while (COMMA) {
        commaTypeParams += nextToken()
        commaTypeParams += annotations(skipNewLines = true, requireOneArgList = false)
        commaTypeParams += typeParam()
      }
      val rbracket = accept(RBRACKET)
      Some(TypeParamClause(typeElementFlatten3(newLineOpt, lbracket, annotations_, typeParam1, commaTypeParams.toList, rbracket)))
    } else
      None
  }

  private def typeBounds(): List[TypeElement] = {
    val superTypeOpt = bound(SUPERTYPE)
    val subTypeOpt = bound(SUBTYPE)
    typeElementFlatten3(superTypeOpt, subTypeOpt)
  }

  private def bound(tokenType: TokenType): Option[(Token, Type)] = {
    if (tokenType) {
      val token = nextToken()
      val type_ = typ()
      Some(token, type_)
    } else
      None
  }

  private def importClause(): ImportClause = {
    val importToken = accept(IMPORT)
    val (importExpr_, otherImportExprs) = commaSeparated(importExpr())
    ImportClause(importToken, importExpr_, otherImportExprs)
  }

  private def importExpr(): ImportExpr = log("importExpr") {
    val initialSelection = if (THIS) {
      val thisToken = nextToken()
      val dot1 = accept(DOT)
      val selector_ = selector()
      val dot2 = accept(DOT)
      makeExpr(thisToken, dot1, selector_, dot2)
    } else {
      val id = ident()
      val dot = accept(DOT)
      if (THIS) {
        val thisToken = nextToken()
        val dot1 = accept(DOT)
        val selector_ = selector()
        val dot2 = accept(DOT)
        makeExpr(id, dot, thisToken, dot1, selector_, dot2)
      } else
        makeExpr(id, dot)
    }

    def loop(idDots: Vector[(Token, Token)]): ImportExpr = {
      if (USCORE) {
        val uscore = nextToken()
        makeExpr(initialSelection, idDots, uscore)
      } else if (LBRACE) {
        val importSelectors_ = importSelectors()
        BlockImportExpr(makeExpr(initialSelection, idDots), importSelectors_)
      } else {
        val id = ident()
        if (DOT) {
          val dot = nextToken()
          loop(idDots :+ (id, dot))
        } else
          makeExpr(initialSelection, idDots, id)
      }
    }
    loop(Vector())
  }

  private def importSelectors(): ImportSelectors = log("importSelectors") {
    val lbrace = accept(LBRACE)
    val importSelectorResult = importSelector()
    var isLast = importSelectorResult._1
    val firstImportSelector = importSelectorResult._2
    val otherImportSelectors = ListBuffer[(Token, Expr)]()
    while (!isLast && COMMA) {
      val commaToken = nextToken()
      val otherImportSelectorResult = importSelector()
      isLast = otherImportSelectorResult._1
      val otherImportSelector = otherImportSelectorResult._2
      otherImportSelectors += ((commaToken, otherImportSelector))
    }
    val rbrace = accept(RBRACE)
    ImportSelectors(lbrace, firstImportSelector, otherImportSelectors.toList, rbrace)
  }

  private def importSelector(): (Boolean, Expr) = log("importSelector") {
    if (USCORE) {
      val uscore = nextToken()
      (true, makeExpr(uscore))
    } else {
      val id = ident()
      val expr_ = if (ARROW) {
        val arrowToken = nextToken()
        if (USCORE) {
          val uscore = nextToken()
          makeExpr(id, arrowToken, uscore)
        } else {
          val id2 = ident()
          makeExpr(id, arrowToken, id2)
        }
      } else {
        makeExpr(id)
      }
      (false, expr_)
    }
  }

  private def defOrDcl(): DefOrDcl = {
    currentTokenType match {
      case VAL ⇒ patDefOrDcl()
      case VAR ⇒ patDefOrDcl()
      case DEF ⇒ funDefOrDcl()
      case TYPE ⇒ typeDefOrDcl()
      case _ ⇒ tmplDef()
    }
  }

  def nonLocalDefOrDcl(): FullDefOrDcl = {
    val annotations_ = annotations(skipNewLines = true, requireOneArgList = false)
    val modifiers_ = modifiers()
    val defOrDcl_ = defOrDcl()
    FullDefOrDcl(annotations_, modifiers_, defOrDcl_)
  }

  private def patDefOrDcl(): PatDefOrDcl = {
    val valOrVarToken = nextToken()
    val pattern_ = pattern2(seqOK = false)
    val otherPatterns = ListBuffer[(Token, Expr)]()
    if (COMMA)
      do {
        val commaToken = nextToken()
        val otherPattern = pattern2(seqOK = false)
        otherPatterns += ((commaToken, otherPattern))
      } while (COMMA)
    val typedOpt_ = typedOpt()
    val equalsClauseOption = if (EQUALS) { // TODO: Check cond
      val equalsToken = accept(EQUALS)
      // Skip USCORE check: will be handled by expr() anyway
      // if (USCORE) { // TODO: check cond 
      //   nextToken()
      // } else

      val clause = expr()
      Some(equalsToken, clause)
    } else
      None
    PatDefOrDcl(valOrVarToken, pattern_, otherPatterns.toList, typedOpt_, equalsClauseOption)
  }

  private def funDefOrDcl(): FunDefOrDcl = {
    val defToken = accept(DEF)
    if (THIS) {
      val thisToken = nextToken()
      val paramClauses_ = paramClauses()
      val newLineOpt_ = newLineOptWhenFollowedBy(LBRACE)
      val funBody = if (LBRACE) {
        val blockExpr_ = constrBlock()
        ProcFunBody(newLineOpt_, blockExpr_)
      } else {
        val equalsToken = accept(EQUALS)
        val constrExpr_ = constrExpr()
        ExprFunBody(equalsToken, constrExpr_)
      }
      FunDefOrDcl(defToken, thisToken, None, paramClauses_, None, Some(funBody))
    } else {
      val nameToken = ident()
      val typeParamClauseOpt_ = typeParamClauseOpt()
      val paramClauses_ = paramClauses()
      val newLineOpt_ = newLineOptWhenFollowedBy(LBRACE)
      val returnTypeOpt = typedOpt()
      val funBodyOpt = if (isStatSep || RBRACE || EOF /* for our tests */ )
        None
      else if (LBRACE) { // TODO: check cond
        val blockExpr_ = blockExpr()
        Some(ProcFunBody(newLineOpt_, blockExpr_))
      } else {
        val (equalsToken, expr_) = equalsExpr()
        Some(ExprFunBody(equalsToken, expr_))
      }
      FunDefOrDcl(defToken, nameToken, typeParamClauseOpt_, paramClauses_, returnTypeOpt, funBodyOpt)
    }
  }

  private def constrExpr(): Expr = {
    if (LBRACE)
      makeExpr(constrBlock())
    else
      selfInvocation()
  }

  private def selfInvocation(): Expr = {
    val thisToken = accept(THIS)
    val newLineOpt_ = newLineOptWhenFollowedBy(LBRACE)
    val argumentExprs_ = argumentExprs()
    val newLineOpt2 = newLineOptWhenFollowedBy(LBRACE)
    var argumentExprsAndNewLines: Vector[(ArgumentExprs, Option[Token])] = Vector()
    while (LPAREN || LBRACE) {
      val argumentExprs_ = argumentExprs()
      val anotherNewLineOpt = newLineOptWhenFollowedBy(LBRACE)
      argumentExprsAndNewLines = argumentExprsAndNewLines :+ ((argumentExprs_, anotherNewLineOpt))
    }
    makeExpr(thisToken, newLineOpt_, argumentExprs_, newLineOpt2, argumentExprsAndNewLines.toList)
  }

  private def constrBlock(): BlockExpr = {
    val lbrace = accept(LBRACE)
    val selfInvocation_ = selfInvocation()
    val statSeq = if (isStatSep) {
      val statSep = nextToken()
      val blockStatSeq_ = blockStatSeq()
      StatSeq(selfReferenceOpt = None, firstStatOpt = Some(selfInvocation_), otherStats = (statSep, blockStatSeq_.firstStatOpt) :: blockStatSeq_.otherStats)
    } else
      StatSeq(selfReferenceOpt = None, firstStatOpt = Some(selfInvocation_), otherStats = Nil)
    val rbrace = accept(RBRACE)
    BlockExpr(lbrace, Right(statSeq), rbrace)
  }

  private def typeDefOrDcl(): TypeDefOrDcl = {
    val typeToken = accept(TYPE)
    val newLinesOpt_ = newLinesOpt()
    val name = ident()
    val typeParamClauseOpt_ = typeParamClauseOpt()
    val extraTypeDclStuff = currentTokenType match {
      case EQUALS ⇒
        val equalsToken = nextToken()
        val typ_ = typ()
        Left(equalsToken, typ_)
      case SUPERTYPE | SUBTYPE | SEMI | NEWLINE | NEWLINES | COMMA | RBRACE | EOF /* <-- for Scalariform tests */ ⇒
        val typeBounds_ = typeBounds()
        Right(typeBounds_)
      case _ ⇒
        throw new ScalaParserException("`=', `>:', or `<:' expected, but got " + currentToken)
    }
    TypeDefOrDcl(typeElementFlatten3(typeToken, newLinesOpt_, name, typeParamClauseOpt_, extraTypeDclStuff))
  }

  private def topLevelTmplDef(): FullDefOrDcl = {
    val annotations_ = annotations(skipNewLines = true, requireOneArgList = false)
    val modifiers_ = modifiers()
    val tmplDef_ = tmplDef()
    FullDefOrDcl(annotations_, modifiers_, tmplDef_)
  }

  private def tmplDef(): TmplDef = {
    currentTokenType match {
      case TRAIT ⇒ classDef()
      case CLASS ⇒ classDef()
      case CASE if lookahead(1) == CLASS ⇒ classDef()
      case OBJECT ⇒ objectDef()
      case CASE if lookahead(1) == OBJECT ⇒ objectDef()
      case _ ⇒ throw new ScalaParserException("expected start of definition, but was " + currentTokenType)
    }
  }

  private def classDef(): TmplDef = {
    var markerTokens: List[Token] = Nil
    if (CASE)
      markerTokens = markerTokens :+ nextToken() // We use two tokens whereas nsc uses CASEOBJECT
    val isTrait: Boolean = TRAIT
    markerTokens = markerTokens :+ nextToken()
    val name = ident()
    val typeParamClauseOpt_ = typeParamClauseOpt()
    val annotations_ = annotations(skipNewLines = false, requireOneArgList = true)
    val (accessModifierOpt_, paramClausesOpt) = if (isTrait)
      (None, None)
    else {
      val accessModifierOpt_ = accessModifierOpt()
      val paramClauses_ = paramClauses()
      (accessModifierOpt_, Some(paramClauses_))
    }
    val templateOpt_ = templateOpt(isTrait)
    TmplDef(markerTokens, name, typeParamClauseOpt_, annotations_, accessModifierOpt_, paramClausesOpt,
      templateOpt_.templateInheritanceSectionOpt, templateOpt_.templateBodyOpt)
  }

  private def objectDef(): TmplDef = {
    var markerTokens: List[Token] = Nil
    if (CASE)
      markerTokens = markerTokens :+ nextToken() // We use two tokens whereas nsc uses CASEOBJECT
    markerTokens = markerTokens :+ accept(OBJECT)
    val name = ident()
    val templateOpt_ = templateOpt(isTrait = false)
    TmplDef(
      markerTokens = markerTokens,
      name = name,
      typeParamClauseOpt = None,
      annotations = Nil,
      accessModifierOpt = None,
      paramClausesOpt = None,
      templateInheritanceSectionOpt = templateOpt_.templateInheritanceSectionOpt,
      templateBodyOption = templateOpt_.templateBodyOpt)
  }

  private def templateParents(isTrait: Boolean): TemplateParents = {
    val type1 = Type(annotType(isPattern = false))
    val argumentExprs_ = ListBuffer[ArgumentExprs]()
    if (LPAREN && !isTrait) {
      do {
        argumentExprs_ += argumentExprs()
      } while (LPAREN)
    }
    val withTypes = ListBuffer[(Token, Type)]()
    while (WITH) {
      val withToken = nextToken()
      val withType = Type(annotType(isPattern = false))
      withTypes += ((withToken, withType))
    }
    TemplateParents(type1, argumentExprs_.toList, withTypes.toList)
  }

  private def template(isTrait: Boolean): Template = {
    val newLineOpt = newLineOptWhenFollowedBy(LBRACE)
    if (LBRACE) {
      val templateBody_ = templateBody().copy(newlineOpt = newLineOpt)
      if (WITH) { // TODO check cond
        val withToken = nextToken()
        val templateParents_ = templateParents(isTrait)
        val templateBodyOpt_ = templateBodyOpt()
        Template(Some(EarlyDefs(templateBody_, Some(withToken))), Some(templateParents_), templateBodyOpt_)
      } else
        Template(Some(EarlyDefs(templateBody_, withOpt = None)), templateParentsOpt = None, templateBodyOpt = None)
    } else {
      val templateParents_ = templateParents(isTrait)
      val templateBodyOpt_ = templateBodyOpt()
      Template(earlyDefsOpt = None, Some(templateParents_), templateBodyOpt_)
    }
  }

  private def templateOpt(isTrait: Boolean): TemplateOpt = {
    if (EXTENDS) {
      val extendsToken = nextToken()
      template(isTrait) match {
        case Template(earlyDefsOpt, templateParentsOpt, templateBodyOpt) ⇒
          TemplateOpt(Some(TemplateInheritanceSection(extendsToken, earlyDefsOpt, templateParentsOpt)), templateBodyOpt)
      }
    } else if (SUBTYPE && isTrait) {
      val subtypeToken = nextToken()
      val template_ = template(isTrait = true)
      template_ match {
        case Template(earlyDefsOpt, templateParentsOpt, templateBodyOpt) ⇒
          TemplateOpt(Some(TemplateInheritanceSection(subtypeToken, earlyDefsOpt, templateParentsOpt)), templateBodyOpt)
      } // TODO: rm duplication with above
    } else {
      // val newLineOpt = newLineOptWhenFollowedBy(LBRACE) // Will be picked up by templateBodyOpt ... TODO: double check this

      val templateBodyOpt_ = templateBodyOpt()
      TemplateOpt(templateInheritanceSectionOpt = None, templateBodyOpt = templateBodyOpt_)
    }
  }

  private def templateBody(): TemplateBody = {
    val lbrace = accept(LBRACE)
    val templateStatSeq_ = templateStatSeq()
    val rbrace = accept(RBRACE)
    val newLineOpt_ = None
    TemplateBody(newLineOpt_, lbrace, templateStatSeq_, rbrace)
  }

  private def templateBodyOpt(): Option[TemplateBody] = {
    val newLineOpt = newLineOptWhenFollowedBy(LBRACE)
    if (LBRACE)
      Some(templateBody().copy(newlineOpt = newLineOpt))
    else if (LPAREN)
      throw new ScalaParserException("traits or objects may not have parametsrs")
    else
      None
  }

  private def refinement(): Refinement = {
    val lbrace = accept(LBRACE)
    val statSeq = refineStatSeq()
    val rbrace = accept(RBRACE)
    Refinement(lbrace, statSeq, rbrace)
  }

  private def packaging(): PrePackageBlock = {
    val packageName = qualId()
    val newLineOpt_ = newLineOptWhenFollowedBy(LBRACE)
    val lbrace = accept(LBRACE)
    val statSeq = topStatSeq()
    val rbrace = accept(RBRACE)
    PrePackageBlock(packageName, newLineOpt_, lbrace, statSeq, rbrace)
  }

  private def topStatSeq(): StatSeq = {
    val statAndStatSeps = ListBuffer[(Option[Stat], Option[Token])]()
    while (!RBRACE && !EOF) {
      val statOpt = if (PACKAGE) {
        val packageToken = nextToken()
        if (OBJECT) {
          val objDef = objectDef()
          Some(FullDefOrDcl(annotations = Nil, modifiers = List(SimpleModifier(packageToken)), defOrDcl = objDef))
        } else
          Some(packaging().complete(packageToken))
      } else if (IMPORT) {
        Some(importClause())
      } else if (CLASS || caseClass || TRAIT || OBJECT || caseObject || AT || isModifier) {
        Some(topLevelTmplDef())
      } else if (!isStatSep)
        throw new ScalaParserException("expected class or object definition")
      else
        None
      val statSepOpt = if (!RBRACE && !EOF) Some(acceptStatSep()) else None
      statAndStatSeps += ((statOpt, statSepOpt))
    }
    rearrangeStatsAndSeps(statAndStatSeps)
  }

  private def templateStatSeq(): StatSeq = {
    val statAndStatSeps = ListBuffer[(Option[Stat], Option[Token])]()

    var selfReferenceOpt = if (isExprIntro) {
      val expr_ = expr(InTemplate)
      if (ARROW) {
        val arrowToken = nextToken()
        Some((expr_, arrowToken))
      } else {
        val statSepOpt = if (!RBRACE && !EOF) Some(acceptStatSep()) else None
        statAndStatSeps += ((Some(expr_), statSepOpt))
        None
      }
    } else
      None

    while (!RBRACE && !EOF) {
      val statOpt = if (IMPORT)
        Some(importClause())
      else if (isExprIntro)
        Some(statement(InTemplate))
      else if (isDefIntro || isModifier || AT)
        Some(nonLocalDefOrDcl())
      else if (!isStatSep)
        throw new ScalaParserException("illegal start of definition: " + currentToken)
      else
        None
      val statSepOpt = if (!RBRACE && !EOF) Some(acceptStatSep()) else None
      statAndStatSeps += ((statOpt, statSepOpt))
    }

    rearrangeStatsAndSeps(statAndStatSeps).copy(selfReferenceOpt = selfReferenceOpt)
  }

  private def refineStatSeq(): StatSeq = {
    val statAndStatSeps = ListBuffer[(Option[Stat], Option[Token])]()
    while (!RBRACE && !EOF) {
      val statOpt =
        if (isDclIntro) {
          val defOrDcl_ = defOrDcl()
          Some(FullDefOrDcl(annotations = Nil, modifiers = Nil, defOrDcl = defOrDcl_))
        } else if (!isStatSep)
          throw new ScalaParserException("illegal start of definition: " + currentToken)
        else
          None
      val statSepOpt = if (!RBRACE) Some(acceptStatSep()) else None
      statAndStatSeps += ((statOpt, statSepOpt))
    }
    rearrangeStatsAndSeps(statAndStatSeps)
  }

  private def localDef(): FullDefOrDcl = {
    val annotations_ = annotations(skipNewLines = true, requireOneArgList = false)
    val localModifiers_ = localModifiers()
    // val modifierCondition = true // TODO: !!!!

    val defOrDcl_ = or(defOrDcl(), tmplDef())
    FullDefOrDcl(annotations_, localModifiers_, defOrDcl_)
  }

  private def blockStatSeq(): StatSeq = {
    val statAndStatSeps = ListBuffer[(Option[Stat], Option[Token])]()
    while (!RBRACE && !EOF && !justCase) {
      if (IMPORT) {
        val importStat = importClause()
        val statSep = acceptStatSep()
        statAndStatSeps += ((Some(importStat), Some(statSep)))
      } else if (isExprIntro) {
        val stat = statement(InBlock)
        val statSepOpt = if (!RBRACE && !justCase) Some(acceptStatSep()) else None
        statAndStatSeps += ((Some(stat), statSepOpt))
      } else if (isDefIntro || isLocalModifier || AT) {
        val defStat: Stat = if (IMPLICIT) {
          val implicitToken = nextToken()
          if (isIdent)
            makeExpr(implicitClosure(InBlock, implicitToken))
          else {
            val localDef_ = localDef()
            localDef_.copy(modifiers = SimpleModifier(implicitToken) :: localDef_.modifiers)
          }
        } else
          localDef()
        val statSepOpt = if (!RBRACE && !justCase) Some(acceptStatSep()) else None
        statAndStatSeps += ((Some(defStat), statSepOpt))
      } else if (isStatSep) {
        val statSep = nextToken()
        statAndStatSeps += ((None, Some(statSep)))
      } else
        throw new ScalaParserException("illegal start of statement: " + currentToken)
    }
    rearrangeStatsAndSeps(statAndStatSeps)
  }

  private def rearrangeStatsAndSeps(statAndStatSeps: Iterable[(Option[Stat], Option[Token])]): StatSeq = {
    var firstStatOpt: Option[Stat] = None
    var firstStatSeen = false
    val otherStats = ListBuffer[(Token, Option[Stat])]()
    var previousStatSepOpt: Option[Token] = None
    for ((statOpt, statSepOpt) ← statAndStatSeps) {
      if (!firstStatSeen) {
        firstStatSeen = true
        firstStatOpt = statOpt
      } else {
        val statSep = previousStatSepOpt.get
        otherStats += ((statSep, statOpt))
      }
      previousStatSepOpt = statSepOpt
    }
    for (statSep ← previousStatSepOpt)
      otherStats += ((statSep, None))
    StatSeq(selfReferenceOpt = None, firstStatOpt = firstStatOpt, otherStats = otherStats.toList)
  }

  def compilationUnit(): CompilationUnit = {
    def topstats(): StatSeq = {
      val initialSemis = ListBuffer[Token]()
      while (SEMI)
        initialSemis += nextToken()

      val otherStatSeq = if (PACKAGE) {
        val packageToken = nextToken()
        if (OBJECT) {
          val objDef = objectDef()
          val packageObjectStat = FullDefOrDcl(annotations = Nil, modifiers = List(SimpleModifier(packageToken)), defOrDcl = objDef)
          if (EOF)
            StatSeq(selfReferenceOpt = None, firstStatOpt = Some(packageObjectStat), otherStats = Nil)
          else {
            val statSep = acceptStatSep()
            val statSeq = topStatSeq()
            StatSeq(selfReferenceOpt = None, firstStatOpt = Some(packageObjectStat), otherStats = (statSep, statSeq.firstStatOpt) :: statSeq.otherStats)
          }
        } else {
          val packageName = qualId()
          val newLineOpt_ = newLineOptWhenFollowedBy(LBRACE)
          if (EOF)
            StatSeq(selfReferenceOpt = None, firstStatOpt = Some(PackageStat(packageToken, packageName)), otherStats = Nil)
          else if (isStatSep) {
            val statSep = nextToken()
            val otherStatSeq = topstats()
            StatSeq(selfReferenceOpt = None, firstStatOpt = Some(PackageStat(packageToken, packageName)), (statSep, otherStatSeq.firstStatOpt) :: otherStatSeq.otherStats)
          } else {
            val lbrace = accept(LBRACE)
            val packageBlockStats = topStatSeq()
            val rbrace = accept(RBRACE)
            val otherStatSeq = topStatSeq()

            val packageBlock = PackageBlock(packageToken, packageName, newLineOpt_, lbrace, packageBlockStats, rbrace)

            if (otherStatSeq.firstStatOpt.nonEmpty)
              throw new ScalaParserException("Expecting semi after package block") // can be non-empty, something of an oddity in the grammar as implemented by 2.8, see 
            // https://lampsvn.epfl.ch/trac/scala/ticket/2973
            StatSeq(None, Some(packageBlock), otherStatSeq.otherStats)
          }
        }
      } else
        topStatSeq()
      if (initialSemis.isEmpty)
        otherStatSeq
      else {
        val otherStats = (initialSemis.init.toList.map((_, None)) :+ (initialSemis.last, otherStatSeq.firstStatOpt)) ++ otherStatSeq.otherStats
        StatSeq(selfReferenceOpt = None, firstStatOpt = None, otherStats = otherStats)
      }
    }
    CompilationUnit(topstats())
  }

  private def xmlStartTag(isPattern: Boolean): XmlStartTag = {
    val startOpen = accept(XML_START_OPEN)
    val name = accept(XML_NAME)
    val attributes = ListBuffer[(Option[Token], XmlAttribute)]()
    var whitespaceOption: Option[Token] = None
    while (!XML_TAG_CLOSE) {
      whitespaceOption = nextTokenIf(XML_WHITESPACE)
      currentTokenType match {
        case XML_NAME ⇒
          val attribute = xmlAttribute(isPattern)
          attributes += ((whitespaceOption, attribute))
          whitespaceOption = None
        case XML_TAG_CLOSE ⇒
        // End loop
        case _ ⇒
          throw new ScalaParserException("Expected XML attribute or end of tag: " + currentToken)
      }
    }
    val tagClose = accept(XML_TAG_CLOSE)
    XmlStartTag(startOpen, name, attributes.toList, whitespaceOption, tagClose)
  }

  private def xmlAttribute(isPattern: Boolean): XmlAttribute = {
    val name = accept(XML_NAME)
    val whitespaceOption = nextTokenIf(XML_WHITESPACE)
    val equals = accept(XML_ATTR_EQ)
    val whitespaceOption2 = nextTokenIf(XML_WHITESPACE)
    val valueOrEmbeddedScala = currentTokenType match {
      case XML_ATTR_VALUE ⇒
        Left(nextToken())
      case LBRACE ⇒
        Right(xmlEmbeddedScala(isPattern))
    }
    XmlAttribute(name, whitespaceOption, equals, whitespaceOption2, valueOrEmbeddedScala)
  }

  private def xmlEmptyElement(isPattern: Boolean): XmlEmptyElement = {
    val startOpen = accept(XML_START_OPEN)
    val name = accept(XML_NAME)
    val attributes = ListBuffer[(Option[Token], XmlAttribute)]()
    var whitespaceOption: Option[Token] = None
    while (!XML_EMPTY_CLOSE) {
      whitespaceOption = nextTokenIf(XML_WHITESPACE)
      currentTokenType match {
        case XML_NAME ⇒
          val attribute = xmlAttribute(isPattern)
          attributes += ((whitespaceOption, attribute))
          whitespaceOption = None
        case XML_EMPTY_CLOSE ⇒
        // End loop
        case _ ⇒
          throw new ScalaParserException("Expected XML attribute or end of tag: " + currentToken)
      }
    }
    val emptyClose = accept(XML_EMPTY_CLOSE)
    XmlEmptyElement(startOpen, name, attributes.toList, whitespaceOption, emptyClose)
  }

  private def xmlEmbeddedScala(isPattern: Boolean): Expr = {
    if (isPattern) {
      val lbrace = accept(LBRACE)
      val pats = patterns(seqOK = true)
      val rbrace = accept(RBRACE)
      makeExpr(lbrace, pats, rbrace)
    } else
      makeExpr(blockExpr())
  }

  private def xmlEndTag(): XmlEndTag = {
    val endOpen = accept(XML_END_OPEN)
    val name = accept(XML_NAME)
    val whitespaceOption = nextTokenIf(XML_WHITESPACE)
    val tagClose = accept(XML_TAG_CLOSE)
    XmlEndTag(endOpen, name, whitespaceOption, tagClose)
  }

  private def xmlNonEmptyElement(isPattern: Boolean): XmlNonEmptyElement = {
    val startTag = xmlStartTag(isPattern)
    val contents = ListBuffer[XmlContents]()
    while (!XML_END_OPEN) {
      val content = currentTokenType match {
        case XML_START_OPEN ⇒ xmlElement(isPattern)
        case XML_PCDATA ⇒ XmlPCDATA(nextToken())
        case XML_COMMENT ⇒ XmlComment(nextToken())
        case XML_CDATA ⇒ XmlCDATA(nextToken())
        case XML_UNPARSED ⇒ XmlUnparsed(nextToken())
        case XML_PROCESSING_INSTRUCTION ⇒ XmlProcessingInstruction(nextToken())
        case LBRACE ⇒ xmlEmbeddedScala(isPattern)
        case _ ⇒ throw new ScalaParserException("Unexpected token in XML: " + currentToken)
      }
      contents += content
    }
    val endTag = xmlEndTag()
    XmlNonEmptyElement(startTag, contents.toList, endTag)
  }

  private def xmlElement(isPattern: Boolean): XmlElement = {
    or(xmlNonEmptyElement(isPattern), xmlEmptyElement(isPattern))
  }

  private def xml(isPattern: Boolean): XmlExpr = {
    def xmlContent(): XmlContents =
      currentTokenType match {
        case XML_START_OPEN ⇒ xmlElement(isPattern)
        case XML_PCDATA ⇒ XmlPCDATA(nextToken())
        case XML_COMMENT ⇒ XmlComment(nextToken())
        case XML_CDATA ⇒ XmlCDATA(nextToken())
        case XML_UNPARSED ⇒ XmlUnparsed(nextToken())
        case XML_PROCESSING_INSTRUCTION ⇒ XmlProcessingInstruction(nextToken())
        case _ ⇒ throw new ScalaParserException("Expected XML: " + currentToken)
      }
    val first = xmlContent()
    val otherContents = ListBuffer[XmlContents]()
    while (XML_START_OPEN || XML_PCDATA) {
      val content = if (XML_START_OPEN)
        xmlElement(isPattern)
      else
        XmlPCDATA(accept(XML_PCDATA))
      otherContents += content
    }
    XmlExpr(first, otherContents.toList)
  }

  private def xmlLiteral() = xml(isPattern = false)

  private def xmlLiteralPattern() = xml(isPattern = true)

  private var tokensArray: Array[Token] = tokens.toArray

  private var pos = 0

  private def currentToken: Token = this(pos)

  private def apply(pos: Int): Token =
    if (pos < tokensArray.length)
      tokensArray(pos)
    else
      tokens.last

  private def currentTokenType = currentToken.tokenType

  /** @return the token before advancing */
  private def nextToken(): Token = {
    val token = currentToken
    pos += 1
    if (logging)
      println("nextToken(): " + token + " --> " + currentToken)
    token
  }

  private def lookahead(n: Int): TokenType = this(pos + n).tokenType

  private implicit def tokenType2Boolean(tokenType: TokenType): Boolean = currentTokenType == tokenType

  private def caseClass = CASE && lookahead(1) == CLASS
  private def caseObject = CASE && lookahead(1) == OBJECT

  private def justCase = CASE && lookahead(1) != CLASS && lookahead(1) != OBJECT

  private abstract sealed class Location
  private case object Local extends Location
  private case object InBlock extends Location
  private case object InTemplate extends Location

  private def isLeftAssoc(token: Token) =
    token.getText.size > 0 && token.getText.last != ':'

  private def isVariableName(name: String): Boolean = {
    val first = name(0)
    ((first.isLower && first.isLetter) || first == '_')
  }

  private def isVarPattern(token: Token) = {
    isIdent(token.tokenType) &&
      isVariableName(token.getText) &&
      !token.getText.startsWith("`")
  }

  private def optional[T](p: ⇒ T): Option[T] =
    or(Some(p), None)

  private def or[T](p1: ⇒ T, p2: ⇒ T): T = {
    val originalPos = pos
    try {
      p1
    } catch {
      case e: ScalaParserException ⇒
        pos = originalPos
        if (logging) println("Rewinding to try alternative: " + currentToken)
        p2
    }
  }

  private def nextTokenIf(tokenType: TokenType): Option[Token] = {
    if (tokenType) {
      val token = currentToken
      nextToken()
      Some(token)
    } else
      None
  }

  private def log[T](s: String)(f: ⇒ T): T = {
    if (logging) {
      println("Enter " + s + " [" + currentToken + "]")
      val result = f
      println("Exit " + s + " [" + currentToken + "]")
      result
    } else
      f
  }

}

object ScalaParser {

  trait ExprElementFlattenable { def elements: List[ExprElement] }
  case class ExprElements(val elements: List[ExprElement]) extends ExprElementFlattenable
  def exprElementFlatten[T <% ExprElementFlattenable]: (T ⇒ List[ExprElement]) = t ⇒ { exprElementFlatten2(t) }
  def exprElementFlatten2[T <% ExprElementFlattenable](t: T): List[ExprElement] = groupGeneralTokens(t.elements)
  def groupGeneralTokens(xs: List[ExprElement]): List[ExprElement] = {
    val eq = (x: ExprElement, y: ExprElement) ⇒ (x, y) match {
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
  implicit def ordinaryPairToExprFlattenable[A <% ExprElementFlattenable, B <% ExprElementFlattenable](pair: (A, B)): ExprElementFlattenable =
    ExprElements(pair._1.elements ::: pair._2.elements)
  implicit def tripleToExprFlattenable[A <% ExprElementFlattenable, B <% ExprElementFlattenable, C <% ExprElementFlattenable](triple: (A, B, C)): ExprElementFlattenable =
    ExprElements(triple._1.elements ::: triple._2.elements ::: triple._3.elements)
  implicit def eitherToExprFlattenable[A <% ExprElementFlattenable, B <% ExprElementFlattenable](either: Either[A, B]): ExprElementFlattenable = ExprElements(either match {
    case Left(x) ⇒ x.elements
    case Right(x) ⇒ x.elements
  })
  implicit def optionToExprFlattenable[T <% ExprElementFlattenable](option: Option[T]): ExprElementFlattenable = option.toList
  implicit def listToExprFlattenable[T <% ExprElementFlattenable](list: List[T]): ExprElementFlattenable = ExprElements(list flatMap { _.elements })
  implicit def vectorToExprFlattenable[T <% ExprElementFlattenable](vector: Vector[T]): ExprElementFlattenable = ExprElements(vector.toList flatMap { _.elements })

  def makeExpr(flattenables: ExprElementFlattenable*): Expr =
    Expr(flattenables.toList flatMap { _.elements })

  trait TypeElementFlattenable { def elements: List[TypeElement] }
  case class TypeElements(val elements: List[TypeElement]) extends TypeElementFlattenable
  def typeElementFlatten[T <% TypeElementFlattenable]: (T ⇒ List[TypeElement]) = _.elements
  def typeElementFlatten2[T <% TypeElementFlattenable](t: T): List[TypeElement] = t.elements
  def typeElementFlatten3(flattenables: TypeElementFlattenable*): List[TypeElement] = flattenables.toList flatMap { _.elements }
  implicit def tokenToTypeFlattenable(token: Token): TypeElementFlattenable = GeneralTokens(List(token))
  implicit def listOfTokenToTypeFlattenable(tokens: List[Token]): TypeElementFlattenable = GeneralTokens(tokens)
  implicit def typeElementToTypeFlattenable(typeElement: TypeElement): TypeElementFlattenable = TypeElements(List(typeElement))
  implicit def eitherToTypeFlattenable[A <% TypeElementFlattenable, B <% TypeElementFlattenable](either: Either[A, B]): TypeElementFlattenable = TypeElements(either match {
    case Left(x) ⇒ x.elements
    case Right(x) ⇒ x.elements
  })
  implicit def pairToTypeFlattenable[A <% TypeElementFlattenable, B <% TypeElementFlattenable](pair: (A, B)): TypeElementFlattenable =
    TypeElements(pair._1.elements ::: pair._2.elements)
  implicit def optionToTypeFlattenable[T <% TypeElementFlattenable](option: Option[T]): TypeElementFlattenable = option.toList
  implicit def listToTypeFlattenable[T <% TypeElementFlattenable](list: List[T]): TypeElementFlattenable = TypeElements(list flatMap { _.elements })

}
