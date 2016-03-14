package scalariform.parser

import scalariform.lexer.Tokens._
import scalariform.lexer._
import PartialFunction._

object InferredSemicolonScalaParser {

  def findSemicolons(tokens: Array[Token]): Set[Token] = {
    val parser = new InferredSemicolonScalaParser(tokens)
    parser.safeParse(parser.compilationUnitOrScript)
    parser.inferredSemicolons
  }

}

class InferredSemicolonScalaParser(tokens: Array[Token]) {

  private val logging: Boolean = false

  private val forgiving = true

  def safeParse[T](production: ⇒ T): Option[T] =
    try Some(production) catch { case e: ScalaParserException ⇒ None }

  def compilationUnitOrScript() {
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

  require(tokens.nonEmpty) // at least EOF

  private def inParens[T](body: ⇒ T) {
    accept(LPAREN)
    body
    accept(RPAREN)
  }

  private def inBraces[T](body: ⇒ T) {
    accept(LBRACE)
    body
    accept(RBRACE)
  }

  private def dropAnyBraces[T](body: ⇒ Any) =
    if (LBRACE)
      inBraces(body)
    else
      body

  private def inBrackets[T](body: ⇒ T) {
    accept(LBRACKET)
    body
    accept(RBRACKET)
  }

  private def makeParens[T](body: ⇒ T) = inParens { if (RPAREN) None else Some(body) }

  private def scriptBody() {
    templateStats()
    accept(EOF)
  }

  private def templateStats() = {
    templateStatSeq()
  }

  private def accept(tokenType: TokenType): Token =
    if (currentTokenType == tokenType)
      nextToken()
    else
      throw new ScalaParserException("Expected token " + tokenType + " but got " + currentToken)

  var inferredSemicolons: Set[Token] = Set()

  private def acceptStatSep(): Token = currentTokenType match {
    case NEWLINE | NEWLINES ⇒
      val token = nextToken()
      inferredSemicolons = inferredSemicolons + token
      token
    case _ ⇒ accept(SEMI)
  }

  private def acceptStatSepOpt() = if (!isStatSeqEnd) acceptStatSep

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
    case VARID | OTHERID | PLUS | MINUS | STAR | PIPE | TILDE | EXCLAMATION ⇒ true
    case _ ⇒ false
  }

  private def isLiteralToken(tokenType: TokenType): Boolean = tokenType match {
    case CHARACTER_LITERAL | INTEGER_LITERAL | FLOATING_POINT_LITERAL |
      STRING_LITERAL | STRING_PART | SYMBOL_LITERAL | TRUE | FALSE | NULL ⇒ true
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

  private def tokenSeparated[T](separator: TokenType, sepFirst: Boolean, part: ⇒ T) {
    if (sepFirst) () else part
    while (separator) {
      nextToken()
      part
    }
  }

  private def commaSeparated[T](part: ⇒ T) =
    tokenSeparated(COMMA, false, part)

  private def caseSeparated[T](part: ⇒ T) = tokenSeparated(CASE, true, part)
  private def readAnnots[T](part: ⇒ T) = tokenSeparated(AT, true, part)

  trait PatternContextSensitive {

    def argType()
    def functionArgType()

    private def tupleInfixType() {
      nextToken()
      if (RPAREN) {
        nextToken()
        accept(ARROW)
        typ()
      } else {
        functionTypes()
        accept(RPAREN)
        if (ARROW) {
          nextToken()
          typ()
        } else {
          simpleTypeRest()
          annotTypeRest()
          compoundTypeRest()
          infixTypeRest()
        }
      }
    }

    private def makeExistentialTypeTree() = refinement()

    def typ() {
      if (LPAREN) tupleInfixType()
      else infixType()

      currentTokenType match {
        case ARROW ⇒
          nextToken()
          typ()
        case FORSOME ⇒
          nextToken()
          makeExistentialTypeTree()
        case _ ⇒
      }
    }

    def typeArgs() = {
      inBrackets(types())
    }

    def annotType() = {
      simpleType()
      annotTypeRest()
    }

    def simpleType() = {
      currentTokenType match {
        case LPAREN ⇒
          inParens(types())
        case USCORE ⇒
          nextToken()
          wildcardType()
        case _ ⇒
          path(thisOK = false, typeOK = true)
      }
      simpleTypeRest()
    }

    private def typeProjection() = {
      nextToken()
      ident()
    }

    private def simpleTypeRest() {
      currentTokenType match {
        case HASH ⇒
          typeProjection()
          simpleTypeRest()
        case LBRACKET ⇒
          typeArgs()
          simpleTypeRest()
        case _ ⇒
          Nil
      }
    }

    def compoundType() = {
      if (LBRACE) None else Some(annotType())
      compoundTypeRest()
    }

    private def compoundTypeRest() = {
      while (WITH) {
        nextToken()
        annotType()
      }
      newLineOptWhenFollowedBy(LBRACE)
      if (LBRACE) Some(refinement()) else None
    }

    def infixTypeRest() {
      if (isIdent && !STAR) {
        val identToken = currentToken
        InfixTypeConstructor(ident())
        newLineOptWhenFollowing(isTypeIntroToken)
        if (isLeftAssoc(identToken)) {
          compoundType()
          infixTypeRest()
        } else {
          infixType()
        }
      } else
        Nil
    }

    def infixType() = {
      compoundType()
      infixTypeRest()
    }

    private def types() =
      commaSeparated(argType())

    private def functionTypes() =
      commaSeparated(functionArgType())

  }

  private def ident() =
    if (isIdent)
      nextToken()
    else
      throw new ScalaParserException("Expected identifier, but got " + currentToken)

  private def selector() = ident()

  private def pathC(thisOK: Boolean, typeOK: Boolean) {
    if (THIS) {
      nextToken()
      if (!thisOK || DOT) {
        accept(DOT)
        selectors(null, typeOK)
      }
    } else if (SUPER) {
      nextToken()
      mixinQualifierOpt()
      accept(DOT)
      selector()
      if (DOT) {
        nextToken()
        selectors(null, typeOK)
      }
    } else {
      ident()
      if (DOT) {
        nextToken()
        if (THIS) {
          nextToken()
          if (!thisOK || DOT) {
            accept(DOT)
            selectors(null, typeOK)
          }
        } else if (SUPER) {
          nextToken()
          mixinQualifierOpt()
          accept(DOT)
          selector()
          if (DOT) {
            nextToken()
            selectors(null, typeOK)
          }
        } else
          selectors(null, typeOK)
      }
    }
  }

  private def path(thisOK: Boolean, typeOK: Boolean) = pathC(thisOK, typeOK)

  private def selectors(delme: String, typeOK: Boolean) {
    if (typeOK && TYPE)
      nextToken()
    else {
      selector()
      if (DOT) {
        nextToken()
        selectors(null, typeOK)
      }
    }
  }

  private def mixinQualifierOpt() {
    if (LBRACKET) inBrackets(ident)
  }

  private def stableId() = path(thisOK = false, typeOK = false)

  private def qualId() = {
    ident()
    if (DOT) {
      nextToken()
      selectors(null, typeOK = false)
    }
  }

  private def pkgQualId() = {
    qualId()
    newLineOptWhenFollowedBy(LBRACE)
  }

  private def literal(inPattern: Boolean = false) =
    if (INTERPOLATION_ID)
      interpolatedString(inPattern)
    else if (CHARACTER_LITERAL || INTEGER_LITERAL || FLOATING_POINT_LITERAL || STRING_LITERAL || SYMBOL_LITERAL || TRUE || FALSE || NULL)
      nextToken()
    else
      throw new ScalaParserException("illegal literal: " + currentToken)

  private def interpolatedString(inPattern: Boolean) {
    nextToken()
    while (STRING_PART) {
      nextToken()
      if (inPattern)
        dropAnyBraces(pattern())
      else if (isIdent)
        ident()
      else if (LBRACE)
        expr()
      else if (THIS)
        nextToken()
      else
        expr()
    }
    if (!STRING_LITERAL) // TODO: Can it be absent, as allowed by Scalac?
      throw new ScalaParserException("Unexpected conclusion to string interpolation: " + currentToken)
    nextToken()
  }

  private def newLineOpt() { if (NEWLINE) nextToken() }

  private def newLinesOpt() = if (NEWLINE || NEWLINES) nextToken()

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

  private def typedOpt() =
    if (COLON) {
      nextToken()
      typ()
    }

  private def typeOrInfixType(location: Location) =
    if (location == Local)
      typ()
    else
      startInfixType()

  private def annotTypeRest() =
    annotations(skipNewLines = false)

  private def wildcardType() = {
    typeBounds()
  }

  private def condExpr() = {
    if (LPAREN) {
      nextToken()
      expr()
      accept(RPAREN)
    } else {
      accept(LPAREN)
      throw new ScalaParserException("Straggling lparen thing")
    }
  }

  private def statement(location: Location) = expr(location)

  def expr() { expr(Local) }

  private def expr(location: Location) {
    expr0(location)
  }

  private def expr0(location: Location) {
    currentTokenType match {
      case IF ⇒
        nextToken()
        condExpr()
        newLinesOpt()
        expr()
        if (SEMI && lookahead(1) == ELSE) Some(nextToken()) else None
        if (ELSE) {
          nextToken()
          expr()
        }

      case TRY ⇒
        nextToken()
        currentTokenType match {
          case LBRACE ⇒
            inBraces(block())
          case LPAREN ⇒ inParens(expr())
          case _      ⇒ expr
        }
        if (!CATCH)
          None
        else {
          nextToken()
          if (!LBRACE)
            expr()
          else {
            inBraces {
              if (CASE)
                caseClauses()
              else
                expr()
            }
          }
        }
        currentTokenType match {
          case FINALLY ⇒
            nextToken()
            expr()
          case _ ⇒
            None
        }

      case WHILE ⇒
        nextToken()
        condExpr()
        newLinesOpt()
        expr()

      case DO ⇒
        nextToken()
        expr()
        if (isStatSep) acceptStatSep() // <-- for inferred semi // nextToken()
        accept(WHILE)
        condExpr()

      case FOR ⇒
        nextToken()
        if (LBRACE) inBraces(enumerators())
        else inParens(enumerators())
        newLinesOpt()
        if (YIELD) {
          nextToken()
          expr()
        } else
          expr()

      case RETURN ⇒
        nextToken()
        if (isExprIntro) expr()

      case THROW ⇒
        nextToken()
        expr()

      case IMPLICIT ⇒
        nextToken()
        List(implicitClosure(location))

      case _ ⇒

        postfixExpr()
        if (EQUALS) {
          optional { /* TODO: case Ident(_) | Select(_, _) | Apply(_, _) => */
            (accept(EQUALS), expr())
          }
        } else if (COLON) {
          nextToken()
          if (USCORE) {
            nextToken()
            accept(STAR)
          } else if (AT) {
            annotations(skipNewLines = false)
          } else {
            typeOrInfixType(location)
          }
        } else if (MATCH) {
          nextToken()
          inBraces(caseClauses())
        }

        val lhsIsTypedParamList = false // TODO!
        if (ARROW && (location != InTemplate || lhsIsTypedParamList)) {
          optional {
            nextToken()
            if (location != InBlock) expr() else block()
          }
        }
    }
  }

  private def implicitClosure(location: Location) {
    ident()
    if (COLON) {
      nextToken()
      typeOrInfixType(location)
    }
    accept(ARROW)
    if (location != InBlock) expr() else block()
  }

  private def postfixExpr() {
    prefixExpr()
    while (isIdent) {
      ident()
      newLineOptWhenFollowing(isExprIntroToken)
      if (isExprIntro) {
        prefixExpr()
      }
    }
  }

  private def prefixExpr() {
    if (isUnaryOp) {
      val isMinus = MINUS
      ident()
      if (isMinus && isNumericLit) {
        literal()
        simpleExprRest(true)
      } else
        simpleExpr()
    } else
      simpleExpr()
  }

  private def simpleExpr() {
    var canApply = true
    if (isLiteral) literal()
    else currentTokenType match {
      case XML_START_OPEN | XML_COMMENT | XML_CDATA | XML_UNPARSED | XML_PROCESSING_INSTRUCTION ⇒
        xmlLiteral()
      case VARID | OTHERID | PLUS | MINUS | STAR | PIPE | TILDE | EXCLAMATION | THIS | SUPER ⇒
        pathC(thisOK = true, typeOK = false)
      case USCORE ⇒
        nextToken()
      case LPAREN ⇒
        makeParens(commaSeparated(expr))
      case LBRACE ⇒
        canApply = false
        blockExpr()
      case NEW ⇒
        canApply = false
        nextToken()
        template()
      case _ ⇒
        throw new ScalaParserException("illegal start of simple expression: " + currentToken)
    }
    simpleExprRest(canApply)
  }

  private def simpleExprRest(canApply: Boolean) {
    if (canApply)
      newLineOptWhenFollowedBy(LBRACE)
    currentTokenType match {
      case DOT ⇒
        nextToken()
        selector()
        simpleExprRest(canApply = true)
      case LBRACKET ⇒
        val identifierCond = true // TODO: missing check: case Ident(_) | Select(_, _) => OK, just means we accept multiple type param [X][Y] clauses
        if (identifierCond) {
          exprTypeArgs()
          simpleExprRest(canApply = true)
        }
      case LPAREN | LBRACE if canApply ⇒
        argumentExprs()
        simpleExprRest(canApply = true)
      case USCORE ⇒
        nextToken()
      case _ ⇒
    }
  }

  private def argumentExprs() {
    def argument() = expr()
    def args() = commaSeparated(argument())
    currentTokenType match {
      case LBRACE ⇒ blockExpr()
      case LPAREN ⇒
        inParens { if (RPAREN) Nil else args() }
      case _ ⇒
    }
  }

  private def multipleArgumentExprs() {
    if (!LPAREN) Nil
    else { argumentExprs(); multipleArgumentExprs() }
  }

  private def blockExpr() {
    inBraces {
      if (justCase) caseClauses()
      else block()
    }
  }

  private def block() { blockStatSeq() }

  private def caseClauses() {
    caseSeparated {
      (pattern(), guard(), caseBlock())
    }
    // TODO:
    //    if (caseClauses_.isEmpty)
    //      accept(CASE)
  }

  private def caseBlock() {
    accept(ARROW)
    block()
  }

  private def guard() {
    if (IF) {
      nextToken()
      postfixExpr()
    }
  }

  private def enumerators() {
    val newStyle = !VAL
    generator(eqOK = false)
    while (isStatSep) {
      acceptStatSep() // <-- for inferred semi //nextToken()
      if (newStyle) {
        if (IF) guard()
        else generator(eqOK = true)
      } else {
        if (VAL) generator(eqOK = true)
        else expr()
      }
    }
  }

  private def generator(eqOK: Boolean) {
    if (VAL) nextToken()
    noSeq.pattern1()
    if (EQUALS && eqOK) nextToken() else accept(LARROW)
    expr()
    while (IF) guard()
  }

  trait SeqContextSensitive extends PatternContextSensitive {

    def isSequenceOK: Boolean

    def isXML: Boolean = false

    def functionArgType() = argType()

    def argType() {
      currentTokenType match {
        case USCORE ⇒
          nextToken()
          if (SUBTYPE || SUPERTYPE) wildcardType()
        case _ if isIdent && isVariableName(currentToken.text) ⇒
          ident()
        case _ ⇒
          typ()
      }
    }

    def patterns() {
      commaSeparated(pattern())
    }

    def pattern() { // Scalac now uses a loop() method, but this is still OK:
      pattern1()
      if (PIPE)
        while (PIPE) {
          nextToken()
          pattern1()
        }
    }

    def pattern1() {
      pattern2()
      if (COLON) {
        nextToken()
        compoundType()
      }
    }

    def pattern2() {
      pattern3()
      if (AT) {
        // TODO: Compare Parsers.scala
        optional {
          nextToken()
          pattern3()
        }
      }
    }

    def pattern3() {
      val firstToken = currentToken
      val secondToken = InferredSemicolonScalaParser.this(pos + 1)
      simplePattern()

      if (isSequenceOK) {
        if (STAR && secondToken == currentToken && firstToken.tokenType == USCORE) {
          lookahead(1) match {
            case RBRACE if isXML ⇒
              nextToken()
              return
            case RPAREN if !isXML ⇒
              nextToken()
              return
            case _ ⇒
          }
        }
      }

      while (isIdent && !PIPE) {
        ident()
        simplePattern()
      }
    }

    def simplePattern() {
      currentTokenType match {
        case VARID | OTHERID | PLUS | MINUS | STAR | PIPE | TILDE | EXCLAMATION | THIS ⇒
          val nameIsMinus: Boolean = MINUS // TODO  case Ident(name) if name == nme.MINUS =>
          stableId()
          condOpt(currentTokenType) {
            case INTEGER_LITERAL | FLOATING_POINT_LITERAL if nameIsMinus ⇒ literal(inPattern = true)
          }
          if (LBRACKET) typeArgs()
          else None
          if (LPAREN) argumentPatterns()
        case USCORE ⇒
          nextToken()
        case CHARACTER_LITERAL | INTEGER_LITERAL | FLOATING_POINT_LITERAL | STRING_LITERAL | SYMBOL_LITERAL | TRUE | FALSE | NULL ⇒
          literal(inPattern = true)
        case LPAREN ⇒
          makeParens(noSeq.patterns)
        case XML_START_OPEN | XML_COMMENT | XML_CDATA | XML_UNPARSED | XML_PROCESSING_INSTRUCTION ⇒
          xmlLiteralPattern()
        case _ ⇒
          throw new ScalaParserException("illegal start of simple pattern: " + currentToken)
      }
    }

  }

  object outPattern extends PatternContextSensitive {
    def argType() = typ()
    def functionArgType() = paramType()
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

  private def argumentPatterns() = {
    inParens { if (RPAREN) Nil else seqPatterns() }
  }

  private def accessQualifierOpt() =
    if (LBRACKET) {
      nextToken()
      if (THIS)
        nextToken()
      else
        ident()
      accept(RBRACKET)
    }

  private def accessModifierOpt() = {
    currentTokenType match {
      case PRIVATE | PROTECTED ⇒
        nextToken()
        accessQualifierOpt()
      case _ ⇒
        None
    }
  }

  private def modifiers() {
    def loop() {
      currentTokenType match {
        case PRIVATE | PROTECTED ⇒
          nextToken()
          accessQualifierOpt()
          loop()
        case ABSTRACT | FINAL | SEALED | OVERRIDE | IMPLICIT | LAZY ⇒
          nextToken()
          loop()
        case NEWLINE ⇒
          nextToken()
          loop()
        case _ ⇒
      }
    }
    loop()
  }

  private def localModifiers() {
    if (isLocalModifier) {
      nextToken()
      localModifiers()
    } else Nil
  }

  private def annotations(skipNewLines: Boolean) {
    readAnnots {
      annotationExpr()
      if (skipNewLines) newLineOpt() else None
    }
  }

  private def constructorAnnotations() =
    readAnnots {
      exprSimpleType()
      argumentExprs()
    }

  private def annotationExpr() {
    exprSimpleType()
    if (LPAREN) multipleArgumentExprs()
  }

  private def paramClauses() {
    var implicitmod = false

    def param() {
      annotations(skipNewLines = false)
      val ownerIsTypeName = true // TODO: if (owner.isTypeName)
      if (ownerIsTypeName) {
        currentTokenType match {
          case VAL | VAR ⇒ nextToken()
          case _         ⇒
        }
      }
      ident()
      if (COLON || !forgiving) {
        accept(COLON)
        paramType()
        if (EQUALS) {
          nextToken()
          expr()
        }
      }
    }

    // Differs from nsc in that we've pulled in lparen/rparen
    def paramClause() {
      accept(LPAREN)
      if (RPAREN) {
        accept(RPAREN)
      } else {
        if (IMPLICIT) {
          nextToken()
          implicitmod = true
        }
        commaSeparated(param())
        accept(RPAREN)
      }
    }

    newLineOptWhenFollowedBy(LPAREN)

    while (!implicitmod && LPAREN) {
      paramClause()
      newLineOptWhenFollowedBy(LPAREN)
    }
  }

  private def paramType() = {
    currentTokenType match {
      case ARROW ⇒
        nextToken()
        typ()
      case _ ⇒
        typ()
        if (STAR)
          nextToken()
    }
  }

  private def typeParamClauseOpt(allowVariance: Boolean) {
    def typeParam() {
      if (allowVariance && isIdent) { // TODO: condition
        if (PLUS)
          nextToken()
        else if (MINUS)
          nextToken()
      }
      wildcardOrIdent()
      typeParamClauseOpt(allowVariance = true)
      typeBounds()
      while (VIEWBOUND) {
        nextToken()
        typ()
      }
      while (COLON) {
        nextToken()
        typ()
      }
    }

    newLineOptWhenFollowedBy(LBRACKET)
    if (LBRACKET) {
      inBrackets(commaSeparated((annotations(skipNewLines = true), typeParam())))
    }
  }

  private def typeBounds() {
    bound(SUPERTYPE)
    bound(SUBTYPE)
  }

  private def bound(tokenType: TokenType) {
    if (tokenType) {
      nextToken()
      typ()
    }
  }

  private def importClause() {
    accept(IMPORT)
    commaSeparated(importExpr())
  }

  private def importExpr() {
    def thisDotted() {
      nextToken()
      accept(DOT)
      selector()
      accept(DOT)
    }
    currentTokenType match {
      case THIS ⇒
        thisDotted()
      case _ ⇒
        ident()
        accept(DOT)
        if (THIS) thisDotted()
    }
    def loop() {
      currentTokenType match {
        case USCORE ⇒
          nextToken()
        case LBRACE ⇒
          importSelectors()
        case _ ⇒
          ident()
          if (DOT) {
            nextToken()
            loop()
          }
      }
    }
    loop()
  }

  private def importSelectors() {
    inBraces(commaSeparated(importSelector()))
  }

  private def wildcardOrIdent() =
    if (USCORE) nextToken()
    else ident()

  private def importSelector() {
    wildcardOrIdent()
    currentTokenType match {
      case ARROW ⇒
        nextToken()
        wildcardOrIdent()
      case _ ⇒
    }
  }

  private def defOrDcl(localDef: Boolean = false) = currentTokenType match {
    case VAL  ⇒ patDefOrDcl()
    case VAR  ⇒ patDefOrDcl()
    case DEF  ⇒ funDefOrDcl(localDef)
    case TYPE ⇒ typeDefOrDcl()
    case _    ⇒ tmplDef()
  }

  def nonLocalDefOrDcl() {
    annotations(skipNewLines = true)
    modifiers()
    defOrDcl()
  }

  private def patDefOrDcl() {
    nextToken()
    commaSeparated(noSeq.pattern2())
    typedOpt()
    if (EQUALS) { // TODO: Check cond
      accept(EQUALS)
      // Skip USCORE check: will be handled by expr() anyway
      // if (USCORE) { // TODO: check cond
      //   nextToken()
      // } else

      expr()
    }
  }

  private def funDefOrDcl(localDef: Boolean) {
    accept(DEF)
    if (THIS) {
      nextToken()
      paramClauses()
      newLineOptWhenFollowedBy(LBRACE)
      currentTokenType match {
        case LBRACE ⇒
          constrBlock()
        case _ ⇒
          accept(EQUALS)
          constrExpr()
      }
    } else {
      ident()
      funDefRest()
    }
  }

  private def funDefRest() {
    typeParamClauseOpt(allowVariance = false)
    paramClauses()
    newLineOptWhenFollowedBy(LBRACE)
    typedOpt()
    if (isStatSep || RBRACE || EOF /* for our tests */ )
      None
    else if (LBRACE) { // TODO: check cond
      blockExpr()
    } else {
      if (!EQUALS) {
        accept(EQUALS)
        throw new AssertionError("Will not reach here")
      }
      nextToken()
      if (VARID && currentToken.text == "macro")
        Some(nextToken())
      else
        None
      expr()
    }
  }

  private def constrExpr() {
    if (LBRACE)
      constrBlock()
    else
      selfInvocation()
  }

  private def selfInvocation() {
    accept(THIS)
    newLineOptWhenFollowedBy(LBRACE)
    argumentExprs()
    newLineOptWhenFollowedBy(LBRACE)
    while (LPAREN || LBRACE) {
      argumentExprs()
      newLineOptWhenFollowedBy(LBRACE)
    }
  }

  private def constrBlock() {
    accept(LBRACE)
    selfInvocation()
    if (isStatSep) {
      acceptStatSep() // <-- for inferred semi // nextToken()
      blockStatSeq()
    }
    accept(RBRACE)
  }

  private def typeDefOrDcl() {
    accept(TYPE)
    newLinesOpt()
    ident()
    typeParamClauseOpt(allowVariance = true)
    currentTokenType match {
      case EQUALS ⇒
        nextToken()
        typ()
      case SUPERTYPE | SUBTYPE | SEMI | NEWLINE | NEWLINES | COMMA | RBRACE | EOF /* <-- for Scalariform tests */ ⇒
        typeBounds()
      case _ ⇒
        throw new ScalaParserException("`=', `>:', or `<:' expected, but got " + currentToken)
    }
  }

  private def topLevelTmplDef() {
    annotations(skipNewLines = true)
    modifiers()
    tmplDef()
  }

  private def tmplDef() {
    currentTokenType match {
      case TRAIT                          ⇒ classDef()
      case CLASS                          ⇒ classDef()
      case CASE if lookahead(1) == CLASS  ⇒ classDef()
      case OBJECT                         ⇒ objectDef()
      case CASE if lookahead(1) == OBJECT ⇒ objectDef()
      case _                              ⇒ throw new ScalaParserException("expected start of definition, but was " + currentToken)
    }
  }

  private def classDef() {
    if (CASE)
      nextToken() // We use two tokens whereas nsc uses CASEOBJECT
    val isTrait: Boolean = TRAIT
    nextToken()
    ident()
    typeParamClauseOpt(allowVariance = true)
    constructorAnnotations()
    if (isTrait)
      (None, None)
    else {
      accessModifierOpt()
      paramClauses()
    }
    templateOpt(isTrait)
  }

  private def objectDef() {
    if (CASE)
      nextToken() // We use two tokens whereas nsc uses CASEOBJECT
    accept(OBJECT)
    ident()
    templateOpt(isTrait = false)
  }

  private def templateParents() {
    def readAppliedParent() {
      startAnnotType()
      if (LPAREN)
        multipleArgumentExprs()
    }
    readAppliedParent()
    while (WITH) {
      nextToken()
      readAppliedParent()
    }
  }

  private def template() {
    newLineOptWhenFollowedBy(LBRACE)
    if (LBRACE) {
      templateBody()
      if (WITH) { // TODO check cond
        nextToken()
        templateParents()
        templateBodyOpt()
      }
    } else {
      templateParents()
      templateBodyOpt()
    }
  }

  private def templateOpt(isTrait: Boolean) {
    if (EXTENDS || SUBTYPE && isTrait) {
      nextToken()
      template()
    } else {
      // val newLineOpt = newLineOptWhenFollowedBy(LBRACE) // Will be picked up by templateBodyOpt ... TODO: double check this
      templateBodyOpt()
    }
  }

  private def templateBody() {
    inBraces(templateStatSeq())
  }

  private def templateBodyOpt() {
    newLineOptWhenFollowedBy(LBRACE)
    if (LBRACE)
      templateBody()
    else if (LPAREN)
      throw new ScalaParserException("traits or objects may not have parameters")
    else
      None
  }

  private def refinement() {
    inBraces(refineStatSeq())
  }

  private def packaging() {
    pkgQualId()
    inBraces(topStatSeq())
  }

  private def topStatSeq() {
    while (!isStatSeqEnd) {
      currentTokenType match {
        case PACKAGE ⇒
          nextToken()
          if (OBJECT) {
            objectDef()
          } else
            packaging()
        case IMPORT ⇒
          importClause()
        case x if x == AT || isTemplateIntro || isModifier ⇒
          topLevelTmplDef()
        case _ ⇒
          if (!isStatSep)
            throw new ScalaParserException("expected class or object definition")
          else
            None
      }
      if (!RBRACE && !EOF)
        acceptStatSep()
    }
  }

  private def templateStatSeq() {

    if (isExprIntro) {
      expr(InTemplate)
      if (ARROW) {
        nextToken()
      } else {
        acceptStatSepOpt()
      }
    } else
      None

    while (!isStatSeqEnd) {
      if (IMPORT)
        Some(importClause())
      else if (isExprIntro)
        Some(statement(InTemplate))
      else if (isDefIntro || isModifier || AT)
        Some(nonLocalDefOrDcl())
      else if (!isStatSep)
        throw new ScalaParserException("illegal start of definition: " + currentToken)
      else
        None
      acceptStatSepOpt()
    }
  }

  private def refineStatSeq() {
    while (!isStatSeqEnd) {
      if (isDclIntro)
        defOrDcl()
      else if (!isStatSep)
        throw new ScalaParserException("illegal start of definition: " + currentToken)

      if (!RBRACE)
        acceptStatSep()
    }
  }

  private def localDef() {
    annotations(skipNewLines = true)
    localModifiers()
    // val modifierCondition = true // TODO: !!!!

    or(defOrDcl(localDef = true), tmplDef())
  }

  private def blockStatSeq() {
    while (!isStatSeqEnd && !justCase) {
      if (IMPORT) {
        importClause()
        acceptStatSep()
      } else if (isExprIntro) {
        statement(InBlock)
        if (!RBRACE && !justCase) Some(acceptStatSep()) else None
      } else if (isDefIntro || isLocalModifier || AT) {
        if (IMPLICIT) {
          nextToken()
          if (isIdent)
            implicitClosure(InBlock)
          else {
            localDef()
          }
        } else
          localDef()
        acceptStatSepOpt()
      } else if (isStatSep) {
        acceptStatSep() // <-- for inferred semi // nextToken()
      } else
        throw new ScalaParserException("illegal start of statement: " + currentToken)
    }
  }

  def compilationUnit() {
    def topstats() {
      while (SEMI)
        nextToken()

      if (PACKAGE) {
        nextToken()
        if (OBJECT) {
          objectDef()
          if (EOF)
            ()
          else {
            acceptStatSep()
            topStatSeq()
          }
        } else {
          pkgQualId()
          if (EOF)
            ()
          else if (isStatSep) {
            acceptStatSep() // <-- to record inferred semi // nextToken()
            topstats()
          } else {
            inBraces(topStatSeq())
            topStatSeq()
          }
        }
      } else
        topStatSeq()
    }
    topstats()
    accept(EOF)
  }

  private def xmlStartTag(isPattern: Boolean) {
    accept(XML_START_OPEN)
    accept(XML_NAME)
    while (!XML_TAG_CLOSE) {
      nextTokenIf(XML_WHITESPACE)
      currentTokenType match {
        case XML_NAME ⇒
          xmlAttribute(isPattern)
        case XML_TAG_CLOSE ⇒
        // End loop
        case _ ⇒
          throw new ScalaParserException("Expected XML attribute or end of tag: " + currentToken)
      }
    }
    accept(XML_TAG_CLOSE)
  }

  private def xmlAttribute(isPattern: Boolean) {
    accept(XML_NAME)
    nextTokenIf(XML_WHITESPACE)
    accept(XML_ATTR_EQ)
    nextTokenIf(XML_WHITESPACE)
    currentTokenType match {
      case XML_ATTR_VALUE ⇒
        nextToken()
      case LBRACE ⇒
        xmlEmbeddedScala(isPattern)
    }
  }

  private def xmlEmptyElement(isPattern: Boolean) {
    accept(XML_START_OPEN)
    accept(XML_NAME)
    while (!XML_EMPTY_CLOSE) {
      nextTokenIf(XML_WHITESPACE)
      currentTokenType match {
        case XML_NAME ⇒
          xmlAttribute(isPattern)
        case XML_EMPTY_CLOSE ⇒
        // End loop
        case _ ⇒
          throw new ScalaParserException("Expected XML attribute or end of tag: " + currentToken)
      }
    }
    accept(XML_EMPTY_CLOSE)
  }

  private def xmlEmbeddedScala(isPattern: Boolean) {
    if (isPattern) {
      accept(LBRACE)
      xmlSeqPatterns()
      accept(RBRACE)
    } else
      blockExpr()
  }

  private def xmlEndTag() {
    accept(XML_END_OPEN)
    accept(XML_NAME)
    nextTokenIf(XML_WHITESPACE)
    accept(XML_TAG_CLOSE)
  }

  private def xmlNonEmptyElement(isPattern: Boolean) {
    xmlStartTag(isPattern)
    while (!XML_END_OPEN) {
      currentTokenType match {
        case XML_START_OPEN             ⇒ xmlElement(isPattern)
        case XML_PCDATA                 ⇒ XmlPCDATA(nextToken())
        case XML_COMMENT                ⇒ XmlComment(nextToken())
        case XML_CDATA                  ⇒ XmlCDATA(nextToken())
        case XML_UNPARSED               ⇒ XmlUnparsed(nextToken())
        case XML_PROCESSING_INSTRUCTION ⇒ XmlProcessingInstruction(nextToken())
        case LBRACE                     ⇒ xmlEmbeddedScala(isPattern)
        case _                          ⇒ throw new ScalaParserException("Unexpected token in XML: " + currentToken)
      }
    }
    xmlEndTag()
  }

  private def xmlElement(isPattern: Boolean) {
    or(xmlNonEmptyElement(isPattern), xmlEmptyElement(isPattern))
  }

  private def xml(isPattern: Boolean) {
    def xmlContent() {
      currentTokenType match {
        case XML_START_OPEN             ⇒ xmlElement(isPattern)
        case XML_PCDATA                 ⇒ XmlPCDATA(nextToken())
        case XML_COMMENT                ⇒ XmlComment(nextToken())
        case XML_CDATA                  ⇒ XmlCDATA(nextToken())
        case XML_UNPARSED               ⇒ XmlUnparsed(nextToken())
        case XML_PROCESSING_INSTRUCTION ⇒ XmlProcessingInstruction(nextToken())
        case _                          ⇒ throw new ScalaParserException("Expected XML: " + currentToken)
      }
      xmlContent()
    }
    while (XML_START_OPEN || XML_PCDATA) {
      if (XML_START_OPEN)
        xmlElement(isPattern)
      else
        XmlPCDATA(accept(XML_PCDATA))
    }
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

  private def isLeftAssoc(token: Token) =
    token.text.nonEmpty && token.text.last != ':'

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
