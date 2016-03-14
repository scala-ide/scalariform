package scalariform.parser

import scalariform.lexer.Tokens._
import scalariform.lexer._
import scalariform.ScalaVersions
import scala.collection.mutable.ListBuffer
import scala.PartialFunction._

class ScalaParser(tokens: Array[Token]) {

  private val logging: Boolean = false

  private val forgiving: Boolean = true

  import ScalaParser._

  def safeParse[T](production: ⇒ T): Option[T] =
    try Some(production) catch { case e: ScalaParserException ⇒ None }

  def compilationUnitOrScript(): CompilationUnit = {
    val originalPos = pos
    try
      compilationUnit()
    catch {
      case e: ScalaParserException ⇒
        pos = originalPos
        if (logging) println("Rewinding to try alternative: " + currentToken)
        try {
          scriptBody()
        } catch { case e2: ScalaParserException ⇒ throw e }
    }
  }

  require(!tokens.isEmpty) // at least EOF

  private def inParens[T](body: ⇒ T): (Token, T, Token) = {
    val openToken = accept(LPAREN)
    val contents = body
    val closeToken = accept(RPAREN)
    (openToken, contents, closeToken)
  }

  private def inBraces[T](body: ⇒ T): (Token, T, Token) = {
    val openToken = accept(LBRACE)
    val contents = body
    val closeToken = accept(RBRACE)
    (openToken, contents, closeToken)
  }

  private def dropAnyBraces[T](body: ⇒ Expr): Expr =
    if (LBRACE) {
      val (lbrace, contents, rbrace) = inBraces(body)
      makeExpr(lbrace, contents, rbrace)
    } else
      body

  private def inBrackets[T](body: ⇒ T): (Token, T, Token) = {
    val openToken = accept(LBRACKET)
    val contents = body
    val closeToken = accept(RBRACKET)
    (openToken, contents, closeToken)
  }

  private def makeParens[T](body: ⇒ T) = inParens { if (RPAREN) None else Some(body) }

  private[scalariform] def scriptBody(): CompilationUnit = {
    val stmts = templateStats()
    val eofToken = accept(EOF)
    CompilationUnit(stmts, eofToken)
  }

  private def templateStats() = {
    templateStatSeq()
  }

  private def accept(tokenType: TokenType): Token =
    if (currentTokenType == tokenType)
      nextToken()
    else
      throw new ScalaParserException("Expected token " + tokenType + " but got " + currentToken)

  private def acceptStatSep(): Token = currentTokenType match {
    case NEWLINE | NEWLINES ⇒ nextToken
    case _                  ⇒ accept(SEMI)
  }

  private def acceptStatSepOpt(): Option[Token] = if (!isStatSeqEnd) Some(acceptStatSep()) else None

  private def isModifier = currentTokenType match {
    case ABSTRACT | FINAL | SEALED | PRIVATE |
      PROTECTED | OVERRIDE | IMPLICIT | LAZY ⇒ true
    case _ ⇒ false
  }

  private def isLocalModifier: Boolean = currentTokenType match {
    case ABSTRACT | FINAL | SEALED | IMPLICIT | LAZY ⇒ true
    case _ ⇒ false
  }

  private def isTemplateIntro: Boolean = currentTokenType match {
    case OBJECT | CLASS | TRAIT ⇒ true
    case CASE if caseObject     ⇒ true
    case CASE if caseClass      ⇒ true
    case _                      ⇒ false
  }

  private def isDclIntro: Boolean = currentTokenType match {
    case VAL | VAR | DEF | TYPE ⇒ true
    case _                      ⇒ false
  }

  private def isDefIntro: Boolean = isTemplateIntro || isDclIntro

  private def isNumericLit: Boolean = currentTokenType match {
    case INTEGER_LITERAL | FLOATING_POINT_LITERAL ⇒ true
    case _                                        ⇒ false
  }

  private def isUnaryOp: Boolean = currentTokenType match {
    case MINUS | PLUS | TILDE | EXCLAMATION ⇒ true
    case _                                  ⇒ false
  }

  private def isIdent: Boolean = isIdent(currentTokenType)

  private def isIdent(tokenType: TokenType) = tokenType match {
    case VARID | OTHERID | PLUS | MINUS | RARROW | STAR | PIPE | TILDE | EXCLAMATION ⇒ true
    case _ ⇒ false
  }

  private def isLiteralToken(tokenType: TokenType): Boolean = tokenType match {
    case CHARACTER_LITERAL | INTEGER_LITERAL | FLOATING_POINT_LITERAL |
      STRING_LITERAL | INTERPOLATION_ID | SYMBOL_LITERAL | TRUE | FALSE | NULL ⇒ true
    case _ ⇒ false
  }

  private def isLiteral = isLiteralToken(currentTokenType)

  private def isExprIntroToken(tokenType: TokenType) =
    isLiteralToken(tokenType) || (tokenType match {
      case THIS | SUPER | IF | FOR | NEW | USCORE | TRY | WHILE |
        DO | RETURN | THROW | LPAREN | LBRACE ⇒ true
      case XML_START_OPEN | XML_UNPARSED | XML_COMMENT | XML_CDATA | XML_PROCESSING_INSTRUCTION ⇒ true
      case _ if isIdent(tokenType) ⇒ true
      case _ ⇒ false
    })

  private def isExprIntro: Boolean = isExprIntroToken(currentTokenType)

  private def isTypeIntroToken(tokenType: TokenType): Boolean = tokenType match {
    case THIS | SUPER | USCORE | LPAREN | AT ⇒ true
    case _ if isIdent(tokenType)             ⇒ true
    case _                                   ⇒ false
  }

  private def isStatSeqEnd = RBRACE || EOF

  private def isStatSep(tokenType: TokenType) =
    tokenType == NEWLINE || tokenType == NEWLINES || tokenType == SEMI

  private def isStatSep: Boolean = isStatSep(currentTokenType)

  private def tokenSeparated[T](separator: TokenType, sepFirst: Boolean, part: ⇒ T): (Option[T], List[(Token, T)]) = {
    val ts = new ListBuffer[(Token, T)]
    val firstOpt = if (sepFirst) None else Some(part)
    while (separator) {
      val separatorToken = nextToken()
      val nextPart = part
      ts += ((separatorToken, nextPart))
    }
    (firstOpt, ts.toList)
  }

  private def commaSeparated[T](part: ⇒ T) =
    tokenSeparated(COMMA, false, part) match { case (firstOpt, rest) ⇒ (firstOpt.get, rest) }

  private def caseSeparated[T](part: ⇒ T) = tokenSeparated(CASE, true, part)._2
  private def readAnnots[T](part: ⇒ T) = tokenSeparated(AT, true, part)._2

  trait PatternContextSensitive {

    def argType(): List[TypeElement]
    def functionArgType(): List[TypeElement]

    private def tupleInfixType() = {
      val lparen = nextToken()
      if (RPAREN) {
        val rparen = nextToken()
        val arrowToken = accept(ARROW)
        val typ_ = typ()
        typeElementFlatten3(lparen, rparen, arrowToken, typ_)
      } else {
        val types_ = functionTypes()
        val rparen = accept(RPAREN)
        val others = if (ARROW) {
          val arrowToken = nextToken()
          val typ_ = typ()
          typeElementFlatten3(arrowToken, typ_)
        } else {
          val simpleTypeRest_ = simpleTypeRest()
          val annotTypeRest_ = annotTypeRest()
          val compoundTypeRest_ = compoundTypeRest()
          val infixTypeRest_ = infixTypeRest()
          typeElementFlatten3(simpleTypeRest_, annotTypeRest_, compoundTypeRest_, infixTypeRest_)
        }
        typeElementFlatten3(lparen, types_, rparen, others)
      }
    }

    private def makeExistentialTypeTree() = refinement()

    def typ(): Type = {
      val others2 =
        if (LPAREN) tupleInfixType()
        else infixType()

      val others3 = currentTokenType match {
        case ARROW ⇒
          val arrowToken = nextToken()
          val typ_ = typ()
          typeElementFlatten3(arrowToken, typ_)
        case FORSOME ⇒
          val forSomeToken = nextToken()
          val refinement_ = makeExistentialTypeTree()
          typeElementFlatten3(forSomeToken, refinement_)
        case _ ⇒
          Nil
      }
      Type(typeElementFlatten3(others2, others3))
    }

    def typeArgs(): List[TypeElement] = {
      val (lbracket, types_, rbracket) = inBrackets(types())
      typeElementFlatten3(lbracket, types_, rbracket)
    }

    def annotType(): List[TypeElement] = {
      val simpleType_ = simpleType()
      val annotTypeRest_ = annotTypeRest()
      typeElementFlatten3(simpleType_, annotTypeRest_)
    }

    def simpleType(): List[TypeElement] = {
      val firstPart = currentTokenType match {
        case LPAREN ⇒
          val (lparen, types_, rparen) = inParens(types())
          typeElementFlatten3(lparen, types_, rparen)
        case USCORE ⇒
          val uscore = nextToken()
          val wildcardType_ = wildcardType()
          typeElementFlatten3(uscore, wildcardType_)
        case _ ⇒
          typeElementFlatten3(path(thisOK = false, typeOK = true))
      }
      val simpleTypeRest_ = simpleTypeRest()
      typeElementFlatten3(firstPart, simpleTypeRest_)
    }

    private def typeProjection() = {
      val hashToken = nextToken()
      val id = ident()
      (hashToken, id)
    }

    private def simpleTypeRest(): List[TypeElement] = currentTokenType match {
      case HASH ⇒
        val (hashToken, id) = typeProjection()
        val simpleTypeRest_ = simpleTypeRest()
        typeElementFlatten3(hashToken, id, simpleTypeRest_)
      case LBRACKET ⇒
        val typeArgs_ = typeArgs()
        val simpleTypeRest_ = simpleTypeRest()
        typeElementFlatten3(typeArgs_, simpleTypeRest_)
      case _ ⇒
        Nil
    }

    def compoundType(): List[TypeElement] = {
      val annotTypeOpt = if (LBRACE) None else Some(annotType())
      val rest = compoundTypeRest()
      typeElementFlatten3(annotTypeOpt, rest)
    }

    private def compoundTypeRest(): List[TypeElement] = {
      val withTypes = ListBuffer[(Token, List[TypeElement])]()
      while (WITH) {
        val withToken = nextToken()
        val annotType_ = annotType()
        withTypes += ((withToken, annotType_))
      }
      val newLineOpt = newLineOptWhenFollowedBy(LBRACE)
      val refinementOpt = if (LBRACE) Some(refinement()) else None
      typeElementFlatten3(withTypes.toList, newLineOpt, refinementOpt)
    }

    def infixTypeRest(): List[TypeElement] = {
      if (isIdent && !STAR) {
        val identToken = currentToken
        val id = InfixTypeConstructor(ident())
        val newLineOpt = newLineOptWhenFollowing(isTypeIntroToken)
        if (isLeftAssoc(identToken)) {
          val compoundType_ = compoundType()
          val infixTypeRest_ = infixTypeRest()
          typeElementFlatten3(id, newLineOpt, compoundType_, infixTypeRest_)
        } else {
          val infixType_ = infixType()
          typeElementFlatten3(id, newLineOpt, infixType_)
        }
      } else
        Nil
    }

    def infixType(): List[TypeElement] = {
      val compoundType_ = compoundType()
      val infixTypeRest_ = infixTypeRest()
      typeElementFlatten3(compoundType_, infixTypeRest_)
    }

    private def types(): List[TypeElement] =
      typeElementFlatten3(commaSeparated(argType()))

    private def functionTypes(): List[TypeElement] =
      typeElementFlatten3(commaSeparated(functionArgType()))

  }

  private def ident(): Token =
    if (isIdent)
      nextToken()
    else
      throw new ScalaParserException("Expected identifier, but got " + currentToken)

  private def selector(): Token = ident()

  private def pathC(thisOK: Boolean, typeOK: Boolean): CallExpr = {
    if (THIS) {
      val thisToken = nextToken()
      val baseCall = CallExpr(None, thisToken)
      if (!thisOK || DOT) {
        val dot = accept(DOT)
        selectors((baseCall, dot), typeOK)
      } else
        baseCall
    } else if (SUPER) {
      val superToken = nextToken()
      val mixinQualifierOpt_ = mixinQualifierOpt()
      val dot = accept(DOT)
      val id = selector()
      val subBaseCall = CallExpr(None, superToken, mixinQualifierOpt_)
      val baseCall = CallExpr(Some((List(subBaseCall), dot)), id)
      if (DOT) {
        val dot2 = nextToken()
        selectors((baseCall, dot2), typeOK)
      } else
        baseCall
    } else {
      val id = ident()
      val baseCall = CallExpr(None, id)
      if (DOT) {
        val dot = nextToken()
        if (THIS) {
          val thisToken = nextToken()
          val baseCall2 = CallExpr(Some((List(baseCall), dot)), thisToken)
          if (!thisOK || DOT) {
            val dot2 = accept(DOT)
            selectors((baseCall2, dot2), typeOK)
          } else
            baseCall2
        } else if (SUPER) {
          val superToken = nextToken()
          val mixinQualifierOpt_ = mixinQualifierOpt()
          val dot2 = accept(DOT)
          val id2 = selector()
          val baseCall2 = CallExpr(Some((List(CallExpr(Some((List(baseCall), dot)), superToken, mixinQualifierOpt_)), dot2)), id2)
          if (DOT) {
            val dot3 = nextToken()
            selectors((baseCall2, dot3), typeOK)
          } else
            baseCall2
        } else
          selectors((baseCall, dot), typeOK)
      } else
        baseCall
    }
  }

  private def path(thisOK: Boolean, typeOK: Boolean): List[Token] = pathC(thisOK, typeOK).tokens

  private def selectors(previousAndDot: (CallExpr, Token), typeOK: Boolean): CallExpr = {
    val exprDotOpt = Some((exprElementFlatten2(previousAndDot._1), previousAndDot._2))
    if (typeOK && TYPE)
      CallExpr(exprDotOpt, nextToken())
    else {
      val id = selector()
      val baseCall = CallExpr(exprDotOpt, id)
      if (DOT) {
        val dot = nextToken()
        selectors((baseCall, dot), typeOK)
      } else
        baseCall
    }
  }

  private def mixinQualifierOpt(): Option[TypeExprElement] =
    if (LBRACKET) Some(TypeExprElement(typeElementFlatten3(inBrackets(ident)))) else None

  private def stableId(): List[Token] = path(thisOK = false, typeOK = false)

  private def qualId(): CallExpr = {
    val id = ident()
    val baseCall = CallExpr(None, id)
    if (DOT) {
      val dot = nextToken()
      selectors((baseCall, dot), typeOK = false)
    } else
      baseCall
  }

  private def pkgQualId(): (CallExpr, Option[Token]) = {
    val pkg = qualId()
    val newLineOpt = newLineOptWhenFollowedBy(LBRACE)
    (pkg, newLineOpt)
  }

  private def literal(inPattern: Boolean = false): List[ExprElement] =
    if (INTERPOLATION_ID)
      List(interpolatedString(inPattern))
    else if (CHARACTER_LITERAL || INTEGER_LITERAL || FLOATING_POINT_LITERAL || STRING_LITERAL || SYMBOL_LITERAL || TRUE || FALSE || NULL)
      exprElementFlatten2(nextToken())
    else
      throw new ScalaParserException("illegal literal: " + currentToken)

  private def interpolatedString(inPattern: Boolean): StringInterpolation = {
    val interpolationId = nextToken()
    val stringPartsAndScala = ListBuffer[(Token, Expr)]()
    while (STRING_PART) {
      val stringPart = nextToken()
      val scalaSegment: Expr =
        if (inPattern)
          dropAnyBraces(pattern())
        else if (isIdent)
          makeExpr(ident())
        else if (LBRACE)
          makeExpr(expr())
        else if (THIS)
          makeExpr(nextToken())
        else
          throw new ScalaParserException("Error in string interpolation: expected block, identifier or `this'")
      stringPartsAndScala += ((stringPart, scalaSegment))
    }
    if (!STRING_LITERAL) // TODO: Can it be absent, as allowed by Scalac?
      throw new ScalaParserException("Unexpected conclusion to string interpolation: " + currentToken)
    val terminalString = nextToken()
    StringInterpolation(interpolationId, stringPartsAndScala.toList, terminalString)
  }

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
    if (COLON)
      Some((nextToken(), typ()))
    else
      None

  private def typeOrInfixType(location: Location): TypeExprElement =
    TypeExprElement(
      if (location == Local)
        typ().contents
      else
        startInfixType()
    )

  private def annotTypeRest(): List[TypeElement] =
    typeElementFlatten3(annotations(skipNewLines = false))

  private def wildcardType(): List[TypeElement] = {
    typeBounds()
  }

  private def condExpr(): CondExpr = {
    if (LPAREN) {
      val lparen = nextToken()
      val expr_ = expr()
      val rparen = accept(RPAREN)
      CondExpr(lparen, expr_, rparen)
    } else {
      accept(LPAREN)
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
        val body: Expr = currentTokenType match {
          case LBRACE ⇒
            val (lbrace, block_, rbrace) = inBraces(block())
            makeExpr(BlockExpr(lbrace, Right(block_), rbrace))
          case LPAREN ⇒ makeExpr(inParens(expr()))
          case _      ⇒ expr
        }
        val catchClauseOption: Option[CatchClause] =
          if (!CATCH)
            None
          else {
            val catchToken = nextToken()
            if (!LBRACE)
              Some(CatchClause(catchToken, Right(expr())))
            else {
              val (lbrace, caseClausesOrStatSeq, rbrace) = inBraces {
                if (CASE)
                  Left(caseClauses())
                else
                  Right(StatSeq(selfReferenceOpt = None, firstStatOpt = Some(expr()), otherStats = Nil))
              }
              Some(CatchClause(catchToken, Left(BlockExpr(lbrace, caseClausesOrStatSeq, rbrace))))
            }
          }
        val finallyClauseOption = currentTokenType match {
          case FINALLY ⇒
            val finallyToken = nextToken()
            val finallyExpr = expr()
            Some((finallyToken, finallyExpr))
          case _ ⇒
            None
        }
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
        val (lParenOrBrace, enumerators_, rParenOrBrace) =
          if (LBRACE) inBraces(enumerators())
          else inParens(enumerators())
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
        exprElementFlatten2((returnToken, returnExpr)) // TODO: <-- use a different type?

      case THROW ⇒
        val throwToken = nextToken()
        val throwExpr = expr()
        exprElementFlatten2((throwToken, throwExpr))

      case IMPLICIT ⇒
        val implicitToken = nextToken()
        List(implicitClosure(location, implicitToken))

      case _ ⇒

        val postfixExpr_ = postfixExpr()
        val intermediateResult = if (EQUALS) {
          optional { /* TODO: case Ident(_) | Select(_, _) | Apply(_, _) => */
            (accept(EQUALS), expr())
          } match {
            case Some((equalsToken, equalsExpr)) ⇒ List(EqualsExpr(postfixExpr_, equalsToken, equalsExpr))
            case None                            ⇒ postfixExpr_
          }
        } else if (COLON) {
          val colonToken = nextToken()
          val rhs = if (USCORE) {
            val uscore = nextToken()
            val star = accept(STAR)
            exprElementFlatten2((uscore, star))
          } else if (AT) {
            annotations(skipNewLines = false)
          } else {
            val type_ = typeOrInfixType(location)
            List(type_)
          }
          List(AscriptionExpr(postfixExpr_, colonToken, rhs))
        } else if (MATCH) {
          val matchToken = nextToken()
          val (lbrace, caseClauses_, rbrace) = inBraces(caseClauses())
          val blockExpr_ = BlockExpr(lbrace, Left(caseClauses_), rbrace)
          List(MatchExpr(postfixExpr_, matchToken, blockExpr_))
        } else
          postfixExpr_

        if (logging)
          println("in expr0, postfixExpr = " + postfixExpr_)

        val lhsIsTypedParamList = cond(postfixExpr_) { case List(ParenExpr(_, _, _)) ⇒ true } // TODO: is this check sufficient?
        if (ARROW && (location != InTemplate || lhsIsTypedParamList)) {
          val anonFuncOpt: Option[List[ExprElement]] = optional {
            val arrowToken = nextToken()
            val postArrow: StatSeq = if (location != InBlock) StatSeq(None, Some(expr()), Nil) else block()
            exprElementFlatten2(AnonymousFunction(intermediateResult, arrowToken, postArrow))
          }
          anonFuncOpt match {
            case None        ⇒ intermediateResult
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
      Some((colonToken, type_))
    } else
      None
    val arrowToken = accept(ARROW)
    val body: StatSeq = if (location != InBlock) StatSeq(None, Some(expr()), Nil) else block()
    AnonymousFunction(exprElementFlatten2((implicitToken, id, colonTypeOpt)), arrowToken, body)
  }

  private final val otherLetters = Set[Char]('\u0024', '\u005F') // '$' and '_'
  private final val letterGroups = {
    import java.lang.Character._
    Set[Byte](LOWERCASE_LETTER, UPPERCASE_LETTER, OTHER_LETTER, TITLECASE_LETTER, LETTER_NUMBER)
  }
  private def isScalaLetter(ch: Char) = letterGroups(java.lang.Character.getType(ch).toByte) || otherLetters(ch)

  private def isOpAssignmentName(name: String) = name match {
    case "!=" | "<=" | ">=" | "" ⇒ false
    case _                       ⇒ name.endsWith("=") && !name.startsWith("=") && Chars.isOperatorPart(name(0))
  }

  private def precedence(id: String) = id(0) match {
    case _ if isOpAssignmentName(id) ⇒ 0
    case c if isScalaLetter(c)       ⇒ 1
    case '|'                         ⇒ 2
    case '^'                         ⇒ 3
    case '&'                         ⇒ 4
    case '=' | '!'                   ⇒ 5
    case '<' | '>'                   ⇒ 6
    case ':'                         ⇒ 7
    case '+' | '-'                   ⇒ 8
    case '*' | '/' | '%'             ⇒ 9
    case _                           ⇒ 10
  }

  private def hasSamePrecedence(token1: Token, token2: Token) = precedence(token1.text) == precedence(token2.text)
  private def hasHigherPrecedence(token1: Token, token2: Token) = precedence(token1.text) > precedence(token2.text)

  private def isRightAssociative(token: Token) = token.text.endsWith(":")

  private object NestedInfixExpr {
    def unapply(infixExpr: InfixExpr) = condOpt(infixExpr) {
      case InfixExpr(List(InfixExpr(x, op1, newLineOpt1, y)), op2, newLineOpt2, z) ⇒ (x, op1, newLineOpt1, y, op2, newLineOpt2, z)
    }
  }

  private def performRotationsForPrecedence(infixExpr: InfixExpr): InfixExpr = infixExpr match {
    case NestedInfixExpr(x, op1, newLineOpt1, y, op2, newLineOpt2, z) if hasHigherPrecedence(op2, op1) ⇒
      InfixExpr(x, op1, newLineOpt1, List(performRotationsForPrecedence(InfixExpr(y, op2, newLineOpt2, z))))
    case _ ⇒ infixExpr
  }

  private def performRotationsForRightAssociativity(infixExpr: InfixExpr): InfixExpr = infixExpr match {
    case NestedInfixExpr(x, op1, newLineOpt1, y, op2, newLineOpt2, z) if hasSamePrecedence(op1, op2) && isRightAssociative(op1) && isRightAssociative(op2) ⇒
      InfixExpr(x, op1, newLineOpt1, List(performRotationsForRightAssociativity(InfixExpr(y, op2, newLineOpt2, z))))
    case _ ⇒ infixExpr
  }

  private def postfixExpr(): List[ExprElement] = {
    var soFar: List[ExprElement] = prefixExpr()
    while (isIdent) {
      val id = ident()
      val newLineOpt = newLineOptWhenFollowing(isExprIntroToken)
      if (isExprIntro) {
        val prefixExpr_ = prefixExpr()
        var infixExpr_ = InfixExpr(soFar, id, newLineOpt, prefixExpr_)
        infixExpr_ = performRotationsForPrecedence(infixExpr_)
        infixExpr_ = performRotationsForRightAssociativity(infixExpr_)
        soFar = List(infixExpr_)
      } else
        soFar = List(PostfixExpr(soFar, id))
    }
    soFar
  }

  private def prefixExpr(): List[ExprElement] = {
    if (isUnaryOp) {
      val isMinus = MINUS
      val unaryId = PrefixExprElement(ident())
      if (isMinus && isNumericLit) {
        val literal_ = literal()
        simpleExprRest(exprElementFlatten2((unaryId, literal_)), true)
      } else
        List(Expr(exprElementFlatten2((unaryId, simpleExpr()))))
    } else
      simpleExpr()
  }

  private def simpleExpr(): List[ExprElement] = {
    var canApply = true
    val firstPart =
      if (isLiteral) exprElementFlatten2(literal())
      else currentTokenType match {
        case XML_START_OPEN | XML_COMMENT | XML_CDATA | XML_UNPARSED | XML_PROCESSING_INSTRUCTION ⇒
          exprElementFlatten2(xmlLiteral())
        case VARID | OTHERID | PLUS | MINUS | STAR | PIPE | TILDE | EXCLAMATION | THIS | SUPER ⇒
          // val callExpr = CallExpr(Some(previousPart, dot), selector_, None, Nil, None)
          List(pathC(thisOK = true, typeOK = false))
        case USCORE ⇒
          exprElementFlatten2(nextToken())
        case LPAREN ⇒
          val (lparen, parenBody, rparen) = makeParens(commaSeparated(expr))
          exprElementFlatten2(ParenExpr(lparen, exprElementFlatten2(parenBody), rparen))
        case LBRACE ⇒
          canApply = false
          exprElementFlatten2(blockExpr())
        case NEW ⇒
          canApply = false
          val newToken = nextToken()
          val template_ = template()
          List(New(newToken, template_))
        case _ ⇒
          throw new ScalaParserException("illegal start of simple expression: " + currentToken)
      }
    simpleExprRest(firstPart, canApply)
  }

  private def simpleExprRest(previousPart: List[ExprElement], canApply: Boolean): List[ExprElement] = {
    val newLineOpt = if (canApply) newLineOptWhenFollowedBy(LBRACE) else None
    currentTokenType match {
      case DOT ⇒
        require(newLineOpt.isEmpty)
        val dot = nextToken()
        val selector_ = selector()
        val callExpr = CallExpr(Some((previousPart, dot)), selector_, None, Nil, None)
        simpleExprRest(List(callExpr), canApply = true)
      case LBRACKET ⇒
        require(newLineOpt.isEmpty)
        val identifierCond = true // TODO: missing check: case Ident(_) | Select(_, _) => OK, just means we accept multiple type param [X][Y] clauses
        if (identifierCond) {
          val typeArgs_ = TypeExprElement(exprTypeArgs())
          val updatedPart = previousPart match {
            case List(callExpr: CallExpr) ⇒
              if (callExpr.typeArgsOpt.isDefined || callExpr.newLineOptsAndArgumentExprss.nonEmpty)
                exprElementFlatten2((previousPart, newLineOpt, typeArgs_)) // TODO: put these into some new type of AST node
              else
                List(callExpr.copy(typeArgsOpt = Some(typeArgs_)))
            case _ ⇒ exprElementFlatten2((previousPart, newLineOpt, typeArgs_))
          }
          simpleExprRest(updatedPart, canApply = true)
        } else
          exprElementFlatten2(previousPart)
      case LPAREN | LBRACE if canApply ⇒
        val argumentExprs_ = argumentExprs().get
        val updatedPart = previousPart match {
          case List(callExpr: CallExpr) ⇒ List(callExpr.copy(newLineOptsAndArgumentExprss = callExpr.newLineOptsAndArgumentExprss :+ ((newLineOpt, argumentExprs_))))
          case _                        ⇒ exprElementFlatten2((previousPart, newLineOpt, argumentExprs_))
        }
        simpleExprRest(updatedPart, canApply = true)
      case USCORE ⇒
        require(newLineOpt.isEmpty)
        val uscore = nextToken()
        previousPart match {
          case List(callExpr: CallExpr) ⇒ List(callExpr.copy(uscoreOpt = Some(uscore)))
          case _                        ⇒ List(PostfixExpr(previousPart, uscore))
        }
      case _ ⇒
        require(newLineOpt.isEmpty)
        previousPart
    }
  }

  /**
   * @return Some(..) if next token is LBRACE or LPAREN
   */
  private def argumentExprs(): Option[ArgumentExprs] = {
    // println("argumentExprs(): " + currentToken)
    def argument() = Argument(expr())
    def args() = commaSeparated(argument())
    condOpt(currentTokenType) {
      case LBRACE ⇒ BlockArgumentExprs(exprElementFlatten2(blockExpr()))
      case LPAREN ⇒
        val (lparen, body, rparen) = inParens { if (RPAREN) Nil else exprElementFlatten2(args()) }
        ParenArgumentExprs(lparen, body, rparen)
    }
  }

  private def multipleArgumentExprs(): List[ArgumentExprs] =
    if (!LPAREN) Nil
    else argumentExprs().get :: multipleArgumentExprs()

  private def blockExpr(): BlockExpr = {
    val (lbrace, body, rbrace) = inBraces {
      if (justCase) Left(caseClauses())
      else Right(block())
    }
    BlockExpr(lbrace, body, rbrace)
  }

  private def block(): StatSeq = blockStatSeq()

  private def caseClauses(): CaseClauses = {
    val caseClauses_ = caseSeparated {
      (pattern(), guard(), caseBlock())
    } map {
      case (caseToken, (pattern_, guardOption, (arrow, blockStatSeq_))) ⇒
        CaseClause(CasePattern(caseToken, pattern_, guardOption, arrow), blockStatSeq_)
    }
    if (caseClauses_.isEmpty)
      accept(CASE)
    CaseClauses(caseClauses_)
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
    val pattern_ = noSeq.pattern1()
    val equalsOrArrowToken = if (EQUALS && eqOK) nextToken() else accept(LARROW)
    val expr_ = expr()
    val guards = ListBuffer[Guard]()
    while (IF) guards += guard().get
    Generator(valOption, pattern_, equalsOrArrowToken, expr_, guards.toList)
  }

  trait SeqContextSensitive extends PatternContextSensitive {

    def isSequenceOK: Boolean

    def isXML: Boolean = false

    def functionArgType() = argType()

    def argType(): List[TypeElement] = currentTokenType match {
      case USCORE ⇒
        val uscore = nextToken()
        val wildcardTypeOpt = if (SUBTYPE || SUPERTYPE) Some(wildcardType()) else None
        typeElementFlatten3(uscore, wildcardTypeOpt)
      case _ if isIdent && isVariableName(currentToken.text) && !(Set(DOT, HASH) contains lookahead(1)) ⇒
        typeElementFlatten3(ident())
      case _ ⇒
        List(typ())
    }

    def patterns(): List[ExprElement] = {
      exprElementFlatten2(commaSeparated(pattern()))
    }

    def pattern(): Expr = { // Scalac now uses a loop() method, but this is still OK:
      val firstPattern = pattern1()
      var currentExpr: ExprElement = firstPattern
      if (PIPE)
        while (PIPE) {
          val pipeToken = nextToken()
          val otherPattern = pattern1()
          currentExpr = InfixExpr(List(currentExpr), pipeToken, None, List(otherPattern))
        }
      makeExpr(currentExpr)
    }

    def pattern1(): Expr = {
      val firstPattern = pattern2()
      val colonTypeOpt = if (COLON) {
        val colonToken = nextToken()
        val compoundType_ = Some(TypeExprElement(compoundType()))
        Some((colonToken, compoundType_))
      } else
        None
      makeExpr(firstPattern, colonTypeOpt)
    }

    def pattern2(): Expr = {
      val firstPattern = pattern3()
      val atOtherOpt = if (AT) {
        // TODO: Compare Parsers.scala
        optional {
          val atToken = nextToken()
          val otherPattern = pattern3()
          (atToken, otherPattern)
        }
      } else
        None
      makeExpr(firstPattern, atOtherOpt)
    }

    def pattern3(): List[ExprElement] = {
      val simplePattern1 = simplePattern()

      if (isSequenceOK) {
        simplePattern1.flatMap(_.tokens).map(_.tokenType) match {
          case List(USCORE) if STAR ⇒
            lookahead(1) match {
              case RBRACE if isXML ⇒
                val starToken = nextToken()
                return exprElementFlatten2((simplePattern1, starToken))
              case RPAREN if !isXML ⇒
                val starToken = nextToken()
                return exprElementFlatten2((simplePattern1, starToken))
              case _ ⇒
            }
          case _ ⇒
        }
      }

      var soFar: List[ExprElement] = simplePattern1
      while (isIdent && !PIPE) {
        val id = ident()
        val otherSimplePattern = simplePattern()
        var infixExpr_ = InfixExpr(soFar, id, None, otherSimplePattern)
        infixExpr_ = performRotationsForPrecedence(infixExpr_)
        infixExpr_ = performRotationsForRightAssociativity(infixExpr_)
        soFar = List(infixExpr_)
      }
      soFar
    }

    def simplePattern(): List[ExprElement] = {
      // println("simplePattern: " + currentToken)
      currentTokenType match {
        case VARID | OTHERID | PLUS | MINUS | STAR | PIPE | TILDE | EXCLAMATION | THIS ⇒
          val nameIsMinus: Boolean = MINUS // TODO  case Ident(name) if name == nme.MINUS =>
          val id = stableId()
          val literalOpt = condOpt(currentTokenType) {
            case INTEGER_LITERAL | FLOATING_POINT_LITERAL if nameIsMinus ⇒ literal(inPattern = true)
          }
          val typeArgsOpt: Option[List[ExprElement]] =
            if (LBRACKET) Some(List(TypeExprElement(typeArgs())))
            else None
          val argumentPatternsOpt = if (LPAREN) Some(argumentPatterns()) else None
          exprElementFlatten2(((id, literalOpt), typeArgsOpt, argumentPatternsOpt))
        case USCORE ⇒
          exprElementFlatten2(nextToken())
        case CHARACTER_LITERAL | INTEGER_LITERAL | FLOATING_POINT_LITERAL | STRING_LITERAL | INTERPOLATION_ID |
          SYMBOL_LITERAL | TRUE | FALSE | NULL ⇒
          exprElementFlatten2(literal(inPattern = true))
        case LPAREN ⇒
          val (lparen, patterns_, rparen) = makeParens(noSeq.patterns)
          exprElementFlatten2((lparen, patterns_, rparen))
        case XML_START_OPEN | XML_COMMENT | XML_CDATA | XML_UNPARSED | XML_PROCESSING_INSTRUCTION ⇒
          exprElementFlatten2(xmlLiteralPattern())
        case _ ⇒
          throw new ScalaParserException("illegal start of simple pattern: " + currentToken)
      }
    }

  }

  object outPattern extends PatternContextSensitive {
    def argType() = List(typ())
    def functionArgType() = List(paramType())
  }

  object seqOK extends SeqContextSensitive {
    val isSequenceOK = true
  }

  object noSeq extends SeqContextSensitive {
    val isSequenceOK = false
  }

  object xmlSeqOK extends SeqContextSensitive {
    val isSequenceOK = true

    override val isXML = true
  }

  def typ() = outPattern.typ()
  def startInfixType() = outPattern.infixType()
  def startAnnotType() = outPattern.annotType()
  def exprTypeArgs() = outPattern.typeArgs()
  def exprSimpleType() = outPattern.simpleType()

  def pattern() = noSeq.pattern()
  def patterns() = noSeq.patterns()
  def seqPatterns() = seqOK.patterns()
  def xmlSeqPatterns() = xmlSeqOK.patterns()

  private def argumentPatterns(): List[ExprElement] = {
    val (lparen, patterns_, rparen) = inParens { if (RPAREN) Nil else seqPatterns() }
    exprElementFlatten2((lparen, patterns_, rparen))
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

  private def localModifiers(): List[Modifier] =
    if (isLocalModifier) SimpleModifier(nextToken()) :: localModifiers()
    else Nil

  private def annotations(skipNewLines: Boolean): List[Annotation] =
    readAnnots {
      val (annotationType, argumentExprss) = annotationExpr()
      val newLineOpt_ = if (skipNewLines) newLineOpt() else None
      (annotationType, argumentExprss, newLineOpt_)
    } map {
      case (atToken, (annotationType, argumentExprss, newLineOpt_)) ⇒
        Annotation(atToken, annotationType, argumentExprss, newLineOpt_)
    }

  private def constructorAnnotations() =
    readAnnots {
      val annotationType = Type(exprSimpleType())
      val argumentExprss = argumentExprs().toList
      (annotationType, argumentExprss)
    } map {
      case (atToken, (annotationType, argumentExprss)) ⇒
        Annotation(atToken, annotationType, argumentExprss, newlineOption = None)
    }

  private def annotationExpr(): (Type, List[ArgumentExprs]) = {
    val annotationType = Type(exprSimpleType())
    val argumentExprss = if (LPAREN) multipleArgumentExprs() else Nil
    (annotationType, argumentExprss)
  }

  private def paramClauses(): ParamClauses = {
    var implicitmod = false

    def param(): Param = {
      val annotations_ = annotations(skipNewLines = false)
      val ownerIsTypeName = true // TODO: if (owner.isTypeName)
      val modifiers_ = ListBuffer[Modifier]()
      val valOrVarOpt = if (ownerIsTypeName) {
        modifiers_ ++= modifiers()
        currentTokenType match {
          case VAL | VAR ⇒ Some(nextToken())
          case _         ⇒ None
        }
      } else
        None
      val id = ident()
      if (COLON || !forgiving) {
        val colonToken = accept(COLON)
        val paramTypeOpt = Some((colonToken, paramType()))
        val defaultValueOpt = if (EQUALS) {
          val equalsToken = nextToken()
          Some((equalsToken, expr()))
        } else
          None
        Param(annotations_, modifiers_.toList, valOrVarOpt, id, paramTypeOpt, defaultValueOpt)
      } else
        Param(annotations_, modifiers_.toList, valOrVarOpt, id, paramTypeOpt = None, defaultValueOpt = None)
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
        val (param_, otherParams) = commaSeparated(param())
        val rparen = accept(RPAREN)
        ParamClause(lparen, implicitOption, Some(param_), otherParams, rparen)
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

  private def paramType(): Type = {
    val typeElements = ListBuffer[TypeElementFlattenable]()
    currentTokenType match {
      case ARROW ⇒
        typeElements += nextToken()
        typeElements += typ()
      case _ ⇒
        typeElements += typ()
        if (STAR)
          typeElements += VarargsTypeElement(nextToken())
    }
    Type(typeElementFlatten3(typeElements.toList))
  }

  private def typeParamClauseOpt(allowVariance: Boolean): Option[TypeParamClause] = {
    def typeParam(): TypeParam = {
      val typeElements = ListBuffer[TypeElementFlattenable]()
      if (allowVariance && isIdent) { // TODO: condition
        if (PLUS)
          typeElements += VarianceTypeElement(nextToken())
        else if (MINUS)
          typeElements += VarianceTypeElement(nextToken())
      }
      typeElements += wildcardOrIdent()
      typeElements += typeParamClauseOpt(allowVariance = true)
      typeElements += typeBounds()
      while (VIEWBOUND) {
        typeElements += nextToken()
        typeElements += typ()
      }
      while (COLON) {
        typeElements += nextToken()
        typeElements += typ()
      }
      TypeParam(typeElementFlatten3(typeElements.toList))
    }

    val newLineOpt = newLineOptWhenFollowedBy(LBRACKET)
    if (LBRACKET) {
      val bracketsContents =
        inBrackets(commaSeparated((annotations(skipNewLines = true), typeParam())))
      Some(TypeParamClause(typeElementFlatten3(newLineOpt, bracketsContents)))
    } else
      None
  }

  private def typeBounds(): List[TypeElement] = {
    val superTypeOpt = bound(SUPERTYPE)
    val subTypeOpt = bound(SUBTYPE)
    typeElementFlatten3(superTypeOpt, subTypeOpt)
  }

  private def bound(tokenType: TokenType): Option[(Token, Type)] = {
    if (tokenType)
      Some((nextToken(), typ()))
    else
      None
  }

  private def importClause(): ImportClause = {
    val importToken = accept(IMPORT)
    val (importExpr_, otherImportExprs) = commaSeparated(importExpr())
    ImportClause(importToken, importExpr_, otherImportExprs)
  }

  private def importExpr(): ImportExpr = {
    def thisDotted(): Expr = {
      val thisToken = nextToken()
      val dot1 = accept(DOT)
      val selector_ = selector()
      val dot2 = accept(DOT)
      makeExpr(thisToken, dot1, selector_, dot2)
    }
    val initialSelection = currentTokenType match {
      case THIS ⇒ thisDotted()
      case _ ⇒
        val id = ident()
        val dot = accept(DOT)
        val thisOpt = if (THIS) Some(thisDotted()) else None
        makeExpr(id, dot, thisOpt)
    }
    def loop(idDots: Vector[(Token, Token)]): ImportExpr = currentTokenType match {
      case USCORE ⇒
        val uscore = nextToken()
        makeExpr(initialSelection, idDots, uscore)
      case LBRACE ⇒
        val importSelectors_ = importSelectors()
        BlockImportExpr(makeExpr(initialSelection, idDots), importSelectors_)
      case _ ⇒
        val id = ident()
        if (DOT) {
          val dot = nextToken()
          loop(idDots :+ ((id, dot)))
        } else
          makeExpr(initialSelection, idDots, id)
    }

    loop(Vector())
  }

  private def importSelectors(): ImportSelectors = {
    val (lbrace, (firstImportSelector, otherImportSelectors), rbrace) = inBraces(commaSeparated(importSelector()))
    ImportSelectors(lbrace, firstImportSelector, otherImportSelectors, rbrace)
  }

  private def wildcardOrIdent(): Token =
    if (USCORE) nextToken()
    else ident()

  private def importSelector(): Expr = {
    val first = wildcardOrIdent()
    currentTokenType match {
      case ARROW ⇒
        val arrowToken = nextToken()
        val rename = wildcardOrIdent()
        makeExpr(first, arrowToken, rename)
      case _ ⇒
        makeExpr(first)
    }
  }

  private def defOrDcl(localDef: Boolean = false): DefOrDcl = currentTokenType match {
    case VAL  ⇒ patDefOrDcl()
    case VAR  ⇒ patDefOrDcl()
    case DEF  ⇒ funDefOrDcl(localDef)
    case TYPE ⇒ typeDefOrDcl()
    case _    ⇒ tmplDef()
  }

  def nonLocalDefOrDcl(): FullDefOrDcl = {
    val annotations_ = annotations(skipNewLines = true)
    val modifiers_ = modifiers()
    val defOrDcl_ = defOrDcl()
    FullDefOrDcl(annotations_, modifiers_, defOrDcl_)
  }

  private def patDefOrDcl(): PatDefOrDcl = {
    val valOrVarToken = nextToken()
    val (pattern_, otherPatterns) = commaSeparated(noSeq.pattern2())
    val typedOpt_ = typedOpt()
    val equalsClauseOption = if (EQUALS) { // TODO: Check cond
      val equalsToken = accept(EQUALS)
      // Skip USCORE check: will be handled by expr() anyway
      // if (USCORE) { // TODO: check cond
      //   nextToken()
      // } else

      val clause = expr()
      Some((equalsToken, clause))
    } else
      None
    PatDefOrDcl(valOrVarToken, pattern_, otherPatterns.toList, typedOpt_, equalsClauseOption)
  }

  private def funDefOrDcl(localDef: Boolean): FunDefOrDcl = {
    val defToken = accept(DEF)
    if (THIS) {
      val thisToken = nextToken()
      val paramClauses_ = paramClauses()
      val newLineOpt_ = newLineOptWhenFollowedBy(LBRACE)
      val funBody = currentTokenType match {
        case LBRACE ⇒
          val blockExpr_ = constrBlock()
          ProcFunBody(newLineOpt_, blockExpr_)
        case _ ⇒
          val equalsToken = accept(EQUALS)
          val constrExpr_ = constrExpr()
          ExprFunBody(equalsToken, None, constrExpr_)
      }
      FunDefOrDcl(defToken, thisToken, None, paramClauses_, None, Some(funBody), localDef)
    } else {
      val nameToken = ident()
      funDefRest(localDef, defToken, nameToken)
    }
  }

  private def funDefRest(localDef: Boolean, defToken: Token, nameToken: Token): FunDefOrDcl = {
    val typeParamClauseOpt_ = typeParamClauseOpt(allowVariance = false)
    val paramClauses_ = paramClauses()
    val newLineOpt_ = newLineOptWhenFollowedBy(LBRACE)
    val returnTypeOpt = typedOpt()
    val funBodyOpt = if (isStatSep || RBRACE || EOF /* for our tests */ )
      None
    else if (LBRACE) { // TODO: check cond
      val blockExpr_ = blockExpr()
      Some(ProcFunBody(newLineOpt_, blockExpr_))
    } else {
      if (!EQUALS) {
        accept(EQUALS)
        throw new AssertionError("Will not reach here")
      }
      val equalsToken = nextToken()
      val macroTokenOpt =
        if (VARID && currentToken.text == "macro")
          Some(nextToken())
        else
          None
      val expr_ = expr()
      Some(ExprFunBody(equalsToken, macroTokenOpt, expr_))
    }
    FunDefOrDcl(defToken, nameToken, typeParamClauseOpt_, paramClauses_, returnTypeOpt, funBodyOpt, localDef)
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
      val argumentExprs_ = argumentExprs().get
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
    val typeParamClauseOpt_ = typeParamClauseOpt(allowVariance = true)
    val extraTypeDclStuff = currentTokenType match {
      case EQUALS ⇒
        val equalsToken = nextToken()
        Left((equalsToken, typ()))
      case SUPERTYPE | SUBTYPE | SEMI | NEWLINE | NEWLINES | COMMA | RBRACE | EOF /* <-- for Scalariform tests */ ⇒
        val typeBounds_ = typeBounds()
        Right(typeBounds_)
      case _ ⇒
        throw new ScalaParserException("`=', `>:', or `<:' expected, but got " + currentToken)
    }
    TypeDefOrDcl(typeElementFlatten3(typeToken, newLinesOpt_, name, typeParamClauseOpt_, extraTypeDclStuff))
  }

  private def topLevelTmplDef(): FullDefOrDcl = {
    val annotations_ = annotations(skipNewLines = true)
    val modifiers_ = modifiers()
    val tmplDef_ = tmplDef()
    FullDefOrDcl(annotations_, modifiers_, tmplDef_)
  }

  private def tmplDef(): TmplDef = {
    currentTokenType match {
      case TRAIT                          ⇒ classDef()
      case CLASS                          ⇒ classDef()
      case CASE if lookahead(1) == CLASS  ⇒ classDef()
      case OBJECT                         ⇒ objectDef()
      case CASE if lookahead(1) == OBJECT ⇒ objectDef()
      case _                              ⇒ throw new ScalaParserException("expected start of definition, but was " + currentToken)
    }
  }

  private def classDef(): TmplDef = {
    var markerTokens: List[Token] = Nil
    if (CASE)
      markerTokens = markerTokens :+ nextToken() // We use two tokens whereas nsc uses CASEOBJECT
    val isTrait: Boolean = TRAIT
    markerTokens = markerTokens :+ nextToken()
    val name = ident()
    val typeParamClauseOpt_ = typeParamClauseOpt(allowVariance = true)
    val annotations_ = constructorAnnotations()
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
      templateBodyOption = templateOpt_.templateBodyOpt
    )
  }

  private def templateParents(): TemplateParents = {
    def readAppliedParent(): (Type, List[ArgumentExprs]) = {
      val parent = Type(startAnnotType())
      val argss =
        if (LPAREN) multipleArgumentExprs()
        else Nil
      (parent, argss)
    }
    val withTypes = ListBuffer[(Token, Type, List[ArgumentExprs])]()
    val (type1, argumentExprs) = readAppliedParent()
    while (WITH) {
      val withToken = nextToken()
      val (type2, argumentExprs2) = readAppliedParent()
      withTypes += ((withToken, type2, argumentExprs2))
    }
    TemplateParents((type1, argumentExprs), withTypes.toList)
  }

  private def template(): Template = {
    val newLineOpt = newLineOptWhenFollowedBy(LBRACE)
    if (LBRACE) {
      val templateBody_ = templateBody().copy(newlineOpt = newLineOpt)
      if (WITH) { // TODO check cond
        val withToken = nextToken()
        val templateParents_ = templateParents()
        val templateBodyOpt_ = templateBodyOpt()
        Template(Some(EarlyDefs(templateBody_, Some(withToken))), Some(templateParents_), templateBodyOpt_)
      } else
        Template(Some(EarlyDefs(templateBody_, withOpt = None)), templateParentsOpt = None, templateBodyOpt = None)
    } else {
      val templateParents_ = templateParents()
      val templateBodyOpt_ = templateBodyOpt()
      Template(earlyDefsOpt = None, Some(templateParents_), templateBodyOpt_)
    }
  }

  private def templateOpt(isTrait: Boolean): TemplateOpt = {
    if (EXTENDS || SUBTYPE && isTrait) {
      val extendsOrSubtypeToken = nextToken()
      template() match {
        case Template(earlyDefsOpt, templateParentsOpt, templateBodyOpt) ⇒
          TemplateOpt(Some(TemplateInheritanceSection(extendsOrSubtypeToken, earlyDefsOpt, templateParentsOpt)), templateBodyOpt)
      }
    } else {
      // val newLineOpt = newLineOptWhenFollowedBy(LBRACE) // Will be picked up by templateBodyOpt ... TODO: double check this

      val templateBodyOpt_ = templateBodyOpt()
      TemplateOpt(templateInheritanceSectionOpt = None, templateBodyOpt = templateBodyOpt_)
    }
  }

  private def templateBody(): TemplateBody = {
    val (lbrace, templateStatSeq_, rbrace) = inBraces(templateStatSeq())
    val newLineOpt_ = None
    TemplateBody(newLineOpt_, lbrace, templateStatSeq_, rbrace)
  }

  private def templateBodyOpt(): Option[TemplateBody] = {
    val newLineOpt = newLineOptWhenFollowedBy(LBRACE)
    if (LBRACE)
      Some(templateBody().copy(newlineOpt = newLineOpt))
    else if (LPAREN)
      throw new ScalaParserException("traits or objects may not have parameters")
    else
      None
  }

  private def refinement(): Refinement = {
    val (lbrace, statSeq, rbrace) = inBraces(refineStatSeq())
    Refinement(lbrace, statSeq, rbrace)
  }

  private def packaging(): PrePackageBlock = {
    val (packageName, newLineOpt_) = pkgQualId()
    val (lbrace, statSeq, rbrace) = inBraces(topStatSeq())
    PrePackageBlock(packageName, newLineOpt_, lbrace, statSeq, rbrace)
  }

  private def topStatSeq(): StatSeq = {
    val statAndStatSeps = ListBuffer[(Option[Stat], Option[Token])]()
    while (!isStatSeqEnd) {
      val statOpt = currentTokenType match {
        case PACKAGE ⇒
          val packageToken = nextToken()
          if (OBJECT) {
            val objDef = objectDef()
            Some(FullDefOrDcl(annotations = Nil, modifiers = List(SimpleModifier(packageToken)), defOrDcl = objDef))
          } else
            Some(packaging().complete(packageToken))
        case IMPORT ⇒
          Some(importClause())
        case x if x == AT || isTemplateIntro || isModifier ⇒
          Some(topLevelTmplDef())
        case _ ⇒
          if (!isStatSep)
            throw new ScalaParserException("expected class or object definition")
          else
            None
      }
      val statSepOpt = if (!RBRACE && !EOF) Some(acceptStatSep()) else None
      statAndStatSeps += ((statOpt, statSepOpt))
    }
    rearrangeStatsAndSeps(statAndStatSeps)
  }

  private def templateStatSeq(): StatSeq = {
    val statAndStatSeps = ListBuffer[(Option[Stat], Option[Token])]()

    val selfReferenceOpt = if (isExprIntro) {
      val expr_ = expr(InTemplate)
      if (ARROW) {
        val arrowToken = nextToken()
        Some((expr_, arrowToken))
      } else {
        val statSepOpt = acceptStatSepOpt()
        statAndStatSeps += ((Some(expr_), statSepOpt))
        None
      }
    } else
      None

    while (!isStatSeqEnd) {
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
      val statSepOpt = acceptStatSepOpt()
      statAndStatSeps += ((statOpt, statSepOpt))
    }

    rearrangeStatsAndSeps(statAndStatSeps).copy(selfReferenceOpt = selfReferenceOpt)
  }

  private def refineStatSeq(): StatSeq = {
    val statAndStatSeps = ListBuffer[(Option[Stat], Option[Token])]()
    while (!isStatSeqEnd) {
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
    val annotations_ = annotations(skipNewLines = true)
    val localModifiers_ = localModifiers()
    // val modifierCondition = true // TODO: !!!!

    val defOrDcl_ = or(defOrDcl(localDef = true), tmplDef())
    FullDefOrDcl(annotations_, localModifiers_, defOrDcl_)
  }

  private def blockStatSeq(): StatSeq = {
    val statAndStatSeps = ListBuffer[(Option[Stat], Option[Token])]()
    while (!isStatSeqEnd && !justCase) {
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
        val statSepOpt = acceptStatSepOpt()
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
          val (packageName, newLineOpt_) = pkgQualId()
          if (EOF)
            StatSeq(selfReferenceOpt = None, firstStatOpt = Some(PackageStat(packageToken, packageName)), otherStats = Nil)
          else if (isStatSep) {
            val statSep = nextToken()
            val otherStatSeq = topstats()
            StatSeq(selfReferenceOpt = None, firstStatOpt = Some(PackageStat(packageToken, packageName)), (statSep, otherStatSeq.firstStatOpt) :: otherStatSeq.otherStats)
          } else {
            val (lbrace, packageBlockStats, rbrace) = inBraces(topStatSeq())
            val otherStatSeq = topStatSeq()
            val packageBlock = PackageBlock(packageToken, packageName, newLineOpt_, lbrace, packageBlockStats, rbrace)
            if (otherStatSeq.selfReferenceOpt.isDefined || otherStatSeq.firstStatOpt.isDefined)
              throw new ScalaParserException("Illegal package blocks") // To avoid blowing up on -ve cases
            StatSeq(None, Some(packageBlock), otherStatSeq.otherStats)
          }
        }
      } else
        topStatSeq()
      if (initialSemis.isEmpty)
        otherStatSeq
      else {
        val otherStats = (initialSemis.init.toList.map((_, None)) :+ ((initialSemis.last, otherStatSeq.firstStatOpt))) ++ otherStatSeq.otherStats
        StatSeq(selfReferenceOpt = None, firstStatOpt = None, otherStats = otherStats)
      }
    }
    val topStats_ = topstats()
    val eofToken = accept(EOF)
    CompilationUnit(topStats_, eofToken)
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
      val pats = xmlSeqPatterns()
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
        case XML_START_OPEN             ⇒ xmlElement(isPattern)
        case XML_PCDATA                 ⇒ XmlPCDATA(nextToken())
        case XML_COMMENT                ⇒ XmlComment(nextToken())
        case XML_CDATA                  ⇒ XmlCDATA(nextToken())
        case XML_UNPARSED               ⇒ XmlUnparsed(nextToken())
        case XML_PROCESSING_INSTRUCTION ⇒ XmlProcessingInstruction(nextToken())
        case LBRACE                     ⇒ xmlEmbeddedScala(isPattern)
        case _                          ⇒ throw new ScalaParserException("Unexpected token in XML: " + currentToken)
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
        case XML_START_OPEN             ⇒ xmlElement(isPattern)
        case XML_PCDATA                 ⇒ XmlPCDATA(nextToken())
        case XML_COMMENT                ⇒ XmlComment(nextToken())
        case XML_CDATA                  ⇒ XmlCDATA(nextToken())
        case XML_UNPARSED               ⇒ XmlUnparsed(nextToken())
        case XML_PROCESSING_INSTRUCTION ⇒ XmlProcessingInstruction(nextToken())
        case _                          ⇒ throw new ScalaParserException("Expected XML: " + currentToken)
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

  private var pos = 0

  private def currentToken: Token = this(pos)

  private def apply(pos: Int): Token =
    if (pos < tokens.length)
      tokens(pos)
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

  private def isLeftAssoc(token: Token) = token.text.nonEmpty && token.text.last != ':'

  private def isVariableName(name: String): Boolean = {
    val first = name(0)
    ((first.isLower && first.isLetter) || first == '_')
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

}

object ScalaParser {

  /**
   * Parse the given text as a compilation unit or script
   * @return None if there is a parse error.
   */
  def parse(text: String, scalaVersion: String = ScalaVersions.DEFAULT_VERSION): Option[AstNode] = {
    val parser = new ScalaParser(ScalaLexer.tokenise(text, scalaVersion = scalaVersion).toArray)
    parser.safeParse(parser.compilationUnitOrScript)
  }

  trait ExprElementFlattenable { def elements: List[ExprElement] }
  case class ExprElements(elements: List[ExprElement]) extends ExprElementFlattenable
  def exprElementFlatten[T <% ExprElementFlattenable]: (T ⇒ List[ExprElement]) = t ⇒ { exprElementFlatten2(t) }
  def exprElementFlatten2[T <% ExprElementFlattenable](t: T): List[ExprElement] = groupGeneralTokens(t.elements)
  def groupGeneralTokens(xs: List[ExprElement]): List[ExprElement] = {
    val eq = (x: ExprElement, y: ExprElement) ⇒ (x, y) match {
      case (GeneralTokens(_), GeneralTokens(_)) ⇒ true
      case _                                    ⇒ false
    }
    val groups = scalariform.utils.Utils.groupBy(eq, xs)
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
    case Left(x)  ⇒ x.elements
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
    case Left(x)  ⇒ x.elements
    case Right(x) ⇒ x.elements
  })
  implicit def pairToTypeFlattenable[A <% TypeElementFlattenable, B <% TypeElementFlattenable](pair: (A, B)): TypeElementFlattenable =
    TypeElements(pair._1.elements ::: pair._2.elements)
  implicit def tripleToTypeFlattenable[A <% TypeElementFlattenable, B <% TypeElementFlattenable, C <% TypeElementFlattenable](triple: (A, B, C)): TypeElementFlattenable =
    TypeElements(triple._1.elements ::: triple._2.elements ::: triple._3.elements)
  implicit def optionToTypeFlattenable[T <% TypeElementFlattenable](option: Option[T]): TypeElementFlattenable = option.toList
  implicit def listToTypeFlattenable[T <% TypeElementFlattenable](list: List[T]): TypeElementFlattenable = TypeElements(list flatMap { _.elements })

}

// Not AST nodes, used as an intermediate structures during parsing:

case class TemplateOpt(templateInheritanceSectionOpt: Option[TemplateInheritanceSection], templateBodyOpt: Option[TemplateBody])

case class PrePackageBlock(name: CallExpr, newlineOpt: Option[Token], lbrace: Token, topStats: StatSeq, rbrace: Token) {
  def complete(packageToken: Token) = PackageBlock(packageToken, name, newlineOpt, lbrace, topStats, rbrace)
}
