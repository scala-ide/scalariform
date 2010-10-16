package scalariform.parser

import scalariform.lexer.Tokens._
import scalariform.lexer._
import scalariform.utils.Utils._
import scala.collection.mutable.ListBuffer

class ScalaRecogniser(tokens: List[Token]) {

  require(!tokens.isEmpty) // at least EOF

  def scriptBody() = {
    val stmts = templateStatSeq()
    accept(EOF)
  }

  private def accept(tokenType: TokenType): Token =
    if (currentTokenType == tokenType)
      nextToken()
    else
      throw new ScalaParserException("Expected token " + tokenType + " but got " + currentToken)

  private def surround[T](open: TokenType, close: TokenType)(f: => T) {
    accept(open)
    f
    accept(close)
  }

  private def acceptStatSep(): Token =
    if (NEWLINE || NEWLINES || SEMI)
      nextToken()
    else
      throw new ScalaParserException("Expected statement separator but got " + currentToken)

  private def isModifier = currentTokenType match {
    case ABSTRACT | FINAL | SEALED | PRIVATE |
      PROTECTED | OVERRIDE | IMPLICIT | LAZY => true
    case _ => false
  }

  private def isLocalModifier: Boolean = currentTokenType match {
    case ABSTRACT | FINAL | SEALED | IMPLICIT | LAZY => true
    case _ => false
  }

  private def isDefIntro: Boolean = currentTokenType match {
    case VAL | VAR | DEF | TYPE | OBJECT | CLASS | TRAIT => true
    case CASE if caseObject => true
    case CASE if caseClass => true
    case _ => false
  }

  private def isDclIntro: Boolean = currentTokenType match {
    case VAL | VAR | DEF | TYPE => true
    case _ => false
  }

  private def isNumericLit: Boolean = currentTokenType match {
    case INTEGER_LITERAL | FLOATING_POINT_LITERAL => true
    case _ => false
  }

  private def isUnaryOp: Boolean = currentTokenType match {
    case MINUS | PLUS | TILDE | EXCLAMATION => true
    case _ => false
  }

  private def isIdent: Boolean = isIdent(currentTokenType)

  private def isIdent(tokenType: TokenType) = tokenType match {
    case VARID | OTHERID | PLUS | MINUS | STAR | PIPE | TILDE | EXCLAMATION => true
    case _ => false
  }

  private def isExprIntroToken(tokenType: TokenType): Boolean = tokenType match {
    case CHARACTER_LITERAL | INTEGER_LITERAL | FLOATING_POINT_LITERAL |
      STRING_LITERAL | SYMBOL_LITERAL | TRUE | FALSE | NULL |
      THIS | SUPER | IF | FOR | NEW | USCORE | TRY | WHILE |
      DO | RETURN | THROW | LPAREN | LBRACE => true
    case XML_START_OPEN | XML_UNPARSED | XML_COMMENT | XML_CDATA | XML_PROCESSING_INSTRUCTION => true
    case _ if isIdent(tokenType) => true
    case _ => false
  }

  private def isExprIntro: Boolean = isExprIntroToken(currentTokenType)

  private def isTypeIntroToken(tokenType: TokenType): Boolean = tokenType match {
    case THIS | SUPER | USCORE | LPAREN | AT => true
    case _ if isIdent(tokenType) => true
    case _ => false
  }

  private def isTypeIntro: Boolean = isTypeIntroToken(currentTokenType)

  private def isStatSep(tokenType: TokenType) =
    tokenType == NEWLINE || tokenType == NEWLINES || tokenType == SEMI

  private def isStatSep: Boolean = isStatSep(currentTokenType)

  private def commaSeparated[T](runParser: => T): (T, List[(Token, T)]) = {
    val ts = new ListBuffer[(Token, T)]
    val first = runParser
    while (COMMA) {
      val commaToken = nextToken()
      val nextPart = runParser
      ts += ((commaToken, nextPart))
    }
    (first, ts.toList)
  }

  private def ident() =
    if (isIdent)
      nextToken()
    else {
      throw new ScalaParserException("Expected identifier, but got " + currentToken)
    }

  private def selector() = ident()

  private def path(thisOK: Boolean, typeOK: Boolean) = {
    if (THIS) {
      nextToken()
      if (!thisOK || DOT) {
        accept(DOT)
        selectors(typeOK)
      }
    } else if (SUPER) {
      nextToken()
      mixinQualifierOpt()
      accept(DOT)
      selector()
      if (DOT) {
        nextToken()
        selectors(typeOK)
      }
    } else {
      ident()
      if (DOT) {
        nextToken()
        if (THIS) {
          nextToken()
          if (!thisOK || DOT) {
            accept(DOT)
            selectors(typeOK)
          }
        } else if (SUPER) {
          nextToken()
          mixinQualifierOpt()
          accept(DOT)
          selector()
          if (DOT) {
            nextToken()
            selectors(typeOK)
          }
        } else
          selectors(typeOK)
      }
    }
  }

  private def selectors(typeOK: Boolean) {
    if (typeOK && TYPE)
      nextToken()
    else {
      selector()
      if (DOT) {
        nextToken()
        selectors(typeOK)
      }
    }
  }

  private def mixinQualifierOpt() =
    if (LBRACKET) {
      nextToken()
      ident()
      accept(RBRACKET)
    }

  private def stableId() = path(thisOK = false, typeOK = false)

  private def qualId() {
    ident()
    if (DOT) {
      nextToken()
      selectors(typeOK = false)
    }
  }

  private def literal() =
    if (CHARACTER_LITERAL || INTEGER_LITERAL || FLOATING_POINT_LITERAL || STRING_LITERAL || SYMBOL_LITERAL || TRUE || FALSE || NULL)
      nextToken()
    else
      throw new ScalaParserException("illegal literal: " + currentToken)

  private def newLineOpt() = if (NEWLINE) nextToken()

  private def newLinesOpt() = if (NEWLINE || NEWLINES) nextToken()

  private def newLineOptWhenFollowedBy(tokenType: TokenType) =
    if (NEWLINE && lookahead(1) == tokenType)
      newLineOpt()

  private def newLineOptWhenFollowing(pred: TokenType => Boolean) =
    if (NEWLINE && pred(lookahead(1)))
      newLineOpt()

  private def typedOpt() =
    if (COLON) {
      nextToken()
      typ()
    }

  private def types(isPattern: Boolean, isTypeApply: Boolean, isFuncArg: Boolean) = {
    commaSeparated(argType(isPattern, isTypeApply, isFuncArg))
  }

  def typ(): Unit = typ(isPattern = false)

  private def typ(isPattern: Boolean) {
    // println("typ(isPattern = " + isPattern + ")  " + currentToken)
    if (LPAREN) {
      nextToken()
      if (RPAREN) {
        nextToken()
        accept(ARROW)
        typ(isPattern)
      } else {
        types(isPattern, isTypeApply = false, isFuncArg = true)
        accept(RPAREN)
        if (ARROW) {
          nextToken()
          typ(isPattern)
        } else {
          simpleTypeRest(isPattern)
          annotTypeRest()
          compoundTypeRest(isPattern)
          infixTypeRest(isPattern)
        }
      }
    } else {
      infixType(isPattern)
    }
    if (ARROW) {
      nextToken()
      typ(isPattern)
    } else if (FORSOME) {
      nextToken()
      refinement()
    }
  }

  private def infixType(isPattern: Boolean) = {
    compoundType(isPattern)
    infixTypeRest(isPattern)
  }

  private def infixTypeRest(isPattern: Boolean) {
    if (isIdent && !STAR) {
      val identToken = currentToken
      ident()
      newLineOptWhenFollowing(isTypeIntroToken)
      if (isLeftAssoc(identToken)) {
        compoundType(isPattern)
        infixTypeRest(isPattern)
      } else {
        infixType(isPattern)
      }
    }
  }

  private def compoundType(isPattern: Boolean) = {
    if (LBRACE)
      ()
    else
      annotType(isPattern)
    compoundTypeRest(isPattern)
  }

  private def compoundTypeRest(isPattern: Boolean) = {
    while (WITH) {
      nextToken()
      annotType(isPattern)
    }
    newLineOptWhenFollowedBy(LBRACE)
    if (LBRACE)
      refinement()
  }

  private def annotType(isPattern: Boolean) = {
    simpleType(isPattern)
    annotTypeRest()
  }

  private def annotTypeRest() = {
    annotations(skipNewLines = false, requireOneArgList = false)
  }

  private def simpleType(isPattern: Boolean) = {
    if (LPAREN) {
      nextToken()
      types(isPattern, isTypeApply = false, isFuncArg = false)
      accept(RPAREN)
    } else if (USCORE) {
      nextToken()
      wildcardType()
    } else {
      path(thisOK = false, typeOK = true)
    }
    simpleTypeRest(isPattern)
  }

  private def simpleTypeRest(isPattern: Boolean) {
    if (HASH) {
      nextToken()
      ident()
      simpleTypeRest(isPattern)
    } else if (LBRACKET) {
      typeArgs(isPattern, isTypeApply = false)
      simpleTypeRest(isPattern)
    }

  }

  private def wildcardType() {
    typeBounds()
  }

  private def typeArgs(isPattern: Boolean, isTypeApply: Boolean) = {
    accept(LBRACKET)
    types(isPattern, isTypeApply, isFuncArg = false)
    accept(RBRACKET)
  }

  private def argType(isPattern: Boolean, isTypeApply: Boolean, isFuncArg: Boolean) = {
    // println("argType: isPattern = " + isPattern + ", " + currentToken )
    if (isPattern) {
      if (USCORE) {
        nextToken()
        if (SUBTYPE || SUPERTYPE)
          wildcardType()
      } else if (isIdent && isVariableName(currentToken.getText)) {
        ident()
      } else
        typ(isPattern = true)

    } else if (isFuncArg) {
      if (ARROW) {
        nextToken()
        typ()
      } else {
        typ()
        if (STAR) {
          nextToken()
        }
      }
    } else
      typ()
  }

  private def equalsExpr() {
    accept(EQUALS)
    expr()
  }

  private def condExpr() {
    if (LPAREN) {
      nextToken()
      expr()
      accept(RPAREN)
    } else
      accept(LPAREN)
  }

  private def statement(location: Location) = expr(location)

  def expr() { expr(Local) }

  private def expr(location: Location) {
    expr0(location)
  }

  private def expr0(location: Location) {
    currentTokenType match {
      case IF =>
        nextToken()
        condExpr()
        newLinesOpt()
        expr()
        if (SEMI && lookahead(1) == ELSE) // nsc has merged the SEMI into the ELSE by this point
          nextToken()
        if (ELSE) {
          nextToken()
          expr()
        }

      case TRY =>
        nextToken()
        if (LBRACE)
          surround(LBRACE, RBRACE)(block())
        else if (LPAREN)
          surround(LPAREN, RPAREN)(expr())
        else
          expr()
        if (CATCH) {
          nextToken()
          surround(LBRACE, RBRACE)(caseClauses())
        }
        if (FINALLY) {
          nextToken()
          expr()
        }

      case WHILE =>
        nextToken()
        condExpr()
        newLinesOpt()
        expr()

      case DO =>
        nextToken()
        expr()
        if (isStatSep)
          nextToken()
        accept(WHILE)
        condExpr()

      case FOR =>

        nextToken()
        val (open, close) = if (LBRACE) (LBRACE, RBRACE) else (LPAREN, RPAREN)
        surround(open, close)(enumerators())
        newLinesOpt()
        if (YIELD) {
          nextToken()
          expr()
        } else
          expr()

      case RETURN =>
        nextToken()
        if (isExprIntro)
          expr()

      case THROW =>
        nextToken()
        expr()

      case IMPLICIT =>
        nextToken()
        implicitClosure(location)

      case _ =>
        postfixExpr()
        if (EQUALS) {
          optional { /* TODO: case Ident(_) | Select(_, _) | Apply(_, _) => */
            nextToken()
            expr()
          }
        } else if (COLON) {
          nextToken()
          if (USCORE) {
            nextToken()
            accept(STAR)
          } else if (AT) {
            annotations(skipNewLines = false, requireOneArgList = false)
          } else {
            if (location == Local) typ() else infixType(isPattern = false)

          }
        } else if (MATCH) {

          nextToken()
          surround(LBRACE, RBRACE)(caseClauses())
        }

        if (ARROW) {
          // val lhsIsTypedParamList = false // TODO: condition
          // if (location != InTemplate || lhsIsTypedParamList)

          optional {
            nextToken()
            if (location != InBlock) expr() else block()
          }
        }
    }
  }

  private def implicitClosure(location: Location) = {
    ident()
    accept(ARROW)
    if (location != InBlock) expr() else block()
  }

  private def postfixExpr() = {
    prefixExpr()
    while (isIdent) {
      ident()
      newLineOptWhenFollowing(isExprIntroToken)
      if (isExprIntro) {
        prefixExpr()
      }

    }

  }

  private def prefixExpr() = {
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

  private def simpleExpr() = {
    var canApply = true
    currentTokenType match {
      case CHARACTER_LITERAL | INTEGER_LITERAL | FLOATING_POINT_LITERAL | STRING_LITERAL | SYMBOL_LITERAL | TRUE | FALSE | NULL => literal()
      case XML_START_OPEN | XML_COMMENT | XML_CDATA | XML_UNPARSED | XML_PROCESSING_INSTRUCTION => xmlLiteral()
      case VARID | OTHERID | PLUS | MINUS | STAR | PIPE | TILDE | EXCLAMATION | THIS | SUPER => path(thisOK = true, typeOK = false)
      case USCORE =>
        nextToken()
      case LPAREN =>
        nextToken
        if (RPAREN)
          ()
        else
          commaSeparated(expr)
        accept(RPAREN)
      case LBRACE =>
        canApply = false
        blockExpr()
      case NEW =>
        canApply = false
        nextToken()
        template(isTrait = false)
      case _ =>
        throw new ScalaParserException("illegal start of simple expression: " + currentToken)
    }
    simpleExprRest(canApply)
  }

  private def simpleExprRest(canApply: Boolean) {
    if (canApply)
      newLineOptWhenFollowedBy(LBRACE)
    currentTokenType match {
      case DOT =>
        nextToken()
        selector()
        simpleExprRest(canApply = true)
      case LBRACKET =>
        val identifierCond = true /* TODO */ /*             case Ident(_) | Select(_, _) => */
        if (identifierCond) {
          typeArgs(isPattern = false, isTypeApply = true)
          simpleExprRest(canApply = true)
        }
      case LPAREN | LBRACE if canApply =>
        argumentExprs()
        simpleExprRest(canApply = true)
      case USCORE =>
        nextToken()
      case _ =>

    }

  }

  private def argumentExprs() = {
    def args() = commaSeparated(expr)
    if (LBRACE)
      blockExpr()
    else
      surround(LPAREN, RPAREN) {
        if (RPAREN)
          ()
        else
          args()
      }
  }

  private def blockExpr() = {
    accept(LBRACE)
    if (justCase) caseClauses()
    else block()
    accept(RBRACE)
  }

  private def block() {
    blockStatSeq()
  }

  private def caseClauses() = {

    do {
      caseClause()
    } while (justCase)
  }

  private def caseClause() {
    accept(CASE)
    pattern()
    guard()
    caseBlock()
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

  private def enumerators() = {
    val newStyle = !VAL
    generator(eqOK = false)
    while (isStatSep) {
      nextToken()
      if (newStyle) {
        if (IF) guard()
        else generator(eqOK = true)
      } else {
        if (VAL) generator(eqOK = true)
        else expr()
      }
    }
  }

  private def generator(eqOK: Boolean) = {
    if (VAL)
      nextToken()
    pattern1(seqOK = false)
    if (EQUALS && eqOK) nextToken()
    else accept(LARROW)
    expr()
    while (IF) guard()

  }

  private def patterns(seqOK: Boolean) =
    commaSeparated(pattern(seqOK))

  private def pattern(seqOK: Boolean) = {
    pattern1(seqOK)
    if (PIPE) {
      while (PIPE) {
        nextToken()
        pattern1(seqOK)
      }
    }
  }

  private def pattern() {
    pattern(seqOK = false)
  }

  private def pattern1(seqOK: Boolean) = {
    pattern2(seqOK)
    if (COLON) { // TODO: case Ident(name) if (treeInfo.isVarPattern(p) && in.token == COLON)
      nextToken()
      compoundType(isPattern = true)
    }
  }

  private def pattern2(seqOK: Boolean) = {
    pattern3(seqOK)
    if (AT) {
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
        nextToken()
        pattern3(seqOK)
      }
    }
  }

  private def pattern3(seqOK: Boolean) {
    simplePattern(seqOK)
    if (seqOK && STAR) {
      nextToken()
      return
    }
    while (isIdent && !PIPE) {
      ident()
      simplePattern(seqOK)
    }
  }

  private def simplePattern(seqOK: Boolean) {
    // println("simplePattern: " + currentToken)
    currentTokenType match {
      case VARID | OTHERID | PLUS | MINUS | STAR | PIPE | TILDE | EXCLAMATION | THIS =>
        val nameIsMinus: Boolean = MINUS // TODO  case Ident(name) if name == nme.MINUS =>
        stableId()
        currentTokenType match {
          case INTEGER_LITERAL | FLOATING_POINT_LITERAL =>
            if (nameIsMinus)
              literal()
          case _ =>
        }
        if (LPAREN)
          argumentPatterns()
      case USCORE =>
        nextToken()
      case CHARACTER_LITERAL | INTEGER_LITERAL | FLOATING_POINT_LITERAL | STRING_LITERAL | SYMBOL_LITERAL | TRUE | FALSE | NULL =>
        literal()
      case LPAREN =>
        nextToken()
        if (RPAREN)
          ()
        else
          patterns(seqOK = false)
        accept(RPAREN)
      case XML_START_OPEN | XML_COMMENT | XML_CDATA | XML_UNPARSED | XML_PROCESSING_INSTRUCTION =>
        xmlLiteralPattern()
      case _ =>
        throw new ScalaParserException("illegal start of simple pattern: " + currentToken)
    }
  }

  private def argumentPatterns() = {
    accept(LPAREN)
    if (RPAREN)
      ()
    else
      patterns(seqOK = true)
    accept(RPAREN)
  }

  private def accessQualifierOpt() = {
    if (LBRACKET) {
      nextToken()
      if (THIS) nextToken()
      else ident()
      accept(RBRACKET)
    }
  }

  private def accessModifierOpt() = {

    currentTokenType match {
      case PRIVATE | PROTECTED =>
        nextToken()
        accessQualifierOpt()
      case _ =>
    }
  }

  private def modifiers() = {
    def loop() {
      currentTokenType match {
        case PRIVATE | PROTECTED =>
          nextToken()
          accessQualifierOpt()
          loop()
        case ABSTRACT | FINAL | SEALED | OVERRIDE | IMPLICIT | LAZY =>
          nextToken()
          loop()
        case NEWLINE =>
          nextToken()
          loop()
        case _ =>
      }
    }
    loop()
  }

  private def localModifiers() {
    def loop() {
      currentTokenType match {
        case ABSTRACT | FINAL | SEALED | IMPLICIT | LAZY =>
          nextToken()
          loop()
        case _ =>
      }
    }
    loop()
  }

  private def annotations(skipNewLines: Boolean, requireOneArgList: Boolean) = {
    while (AT) {
      nextToken()
      annotationExpr(requireOneArgList)
      if (skipNewLines) newLineOpt()
    }
  }

  private def annotationExpr(requireOneArgList: Boolean) {
    simpleType(isPattern = false)
    if (requireOneArgList)
      argumentExprs()
    else if (LPAREN)
      do { argumentExprs() } while (LPAREN)
  }

  private def paramClauses() {
    var implicitmod = false

    def param() {
      annotations(skipNewLines = false, requireOneArgList = false)
      val ownerIsTypeName = true // TODO: if (owner.isTypeName)
      if (ownerIsTypeName) {
        modifiers()
        if (VAL)
          nextToken()
        else if (VAR)
          nextToken()
        else if (VAL) // TODO: condition
          accept(VAL)
      }
      ident()
      accept(COLON)
      paramType()
      if (EQUALS) {
        nextToken()
        expr()
      }
    }

    def paramClause() {
      if (!RPAREN) {
        if (IMPLICIT) {
          nextToken()
          implicitmod = true
        }
        param()
        while (COMMA) {
          nextToken()
          param()
        }
      }
    }

    newLineOptWhenFollowedBy(LPAREN)

    while (!implicitmod && LPAREN) {
      nextToken()
      paramClause()
      accept(RPAREN)
      newLineOptWhenFollowedBy(LPAREN)
    }

  }

  private def paramType() = {
    if (ARROW) {
      nextToken()
      typ()
    } else {
      typ()
      if (STAR) {
        nextToken()
      }
    }
  }

  private def typeParamClauseOpt() {

    def typeParam() {
      if (isIdent) { // TODO: condition 
        if (PLUS)
          nextToken()
        else if (MINUS)
          nextToken()
      }
      if (USCORE)
        nextToken()
      else
        ident()
      typeParamClauseOpt()
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
      nextToken()
      annotations(skipNewLines = true, requireOneArgList = false)
      typeParam()
      while (COMMA) {
        nextToken()
        annotations(skipNewLines = true, requireOneArgList = false)
        typeParam()
      }
      accept(RBRACKET)
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
    if (THIS) {
      nextToken()
      accept(DOT)
      selector()
      accept(DOT)
    } else {
      ident()
      accept(DOT)
      if (THIS) {
        nextToken()
        accept(DOT)
        selector()
        accept(DOT)
      }
    }

    def loop() {
      if (USCORE) {
        nextToken()
      } else if (LBRACE) {
        importSelectors()
      } else {

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
    accept(LBRACE)
    var isLast = importSelector()
    while (!isLast && COMMA) {
      nextToken()
      isLast = importSelector()
    }
    accept(RBRACE)
  }

  private def importSelector(): Boolean = {
    if (USCORE) {
      nextToken()
      true
    } else {
      ident()
      if (ARROW) {
        nextToken()
        if (USCORE) {
          nextToken()
        } else {
          ident()
        }
      }
      false
    }
  }

  private def defOrDcl() {
    currentTokenType match {
      case VAL => patDefOrDcl()
      case VAR => patDefOrDcl()
      case DEF => funDefOrDcl()
      case TYPE => typeDefOrDcl()
      case _ => tmplDef()
    }

  }

  def nonLocalDefOrDcl() = {
    annotations(skipNewLines = true, requireOneArgList = false)
    modifiers()
    defOrDcl()
  }

  private def patDefOrDcl() = {
    do {
      nextToken()
      pattern2(seqOK = false)
    } while (COMMA)

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

  private def funDefOrDcl() = {
    nextToken()
    if (THIS) {
      nextToken()
      paramClauses()
      newLineOptWhenFollowedBy(LBRACE)
      if (LBRACE)
        constrBlock()
      else {
        accept(EQUALS)
        constrExpr()
      }
    } else {
      ident()
      typeParamClauseOpt()
      paramClauses()
      newLineOptWhenFollowedBy(LBRACE)
      typedOpt()
      if (isStatSep || RBRACE || EOF /* for our tests */ ) {} else if (LBRACE) { // TODO: check cond
        blockExpr()
      } else {
        equalsExpr()
      }
    }

  }

  private def constrExpr() = {
    if (LBRACE)
      constrBlock()
    else
      selfInvocation()
  }

  private def selfInvocation() = {
    accept(THIS)
    newLineOptWhenFollowedBy(LBRACE)
    argumentExprs()
    newLineOptWhenFollowedBy(LBRACE)
    while (LPAREN || LBRACE) {
      argumentExprs()
      newLineOptWhenFollowedBy(LBRACE)
    }
  }

  private def constrBlock() = {
    nextToken()
    selfInvocation()
    if (isStatSep) {
      nextToken()
      blockStatSeq()
    }
    accept(RBRACE)
  }

  private def typeDefOrDcl() = {
    nextToken()
    newLinesOpt()
    ident()
    typeParamClauseOpt()
    currentTokenType match {
      case EQUALS =>
        nextToken()
        typ()
      case SUPERTYPE | SUBTYPE | SEMI | NEWLINE | NEWLINES | COMMA | RBRACE | EOF /* <-- for Scalariform tests */ =>
        typeBounds()
      case _ =>
        throw new ScalaParserException("`=', `>:', or `<:' expected, but got " + currentToken)
    }
  }

  private def topLevelTmplDef() = {
    annotations(skipNewLines = true, requireOneArgList = false)
    modifiers()
    tmplDef()
  }

  private def tmplDef() = {
    currentTokenType match {
      case TRAIT => classDef()
      case CLASS => classDef()
      case CASE if lookahead(1) == CLASS => classDef()
      case OBJECT => objectDef()
      case CASE if lookahead(1) == OBJECT => objectDef()
      case _ => throw new ScalaParserException("expected start of definition, but was " + currentTokenType)
    }

  }

  private def classDef() = {
    if (CASE) nextToken() // We use two tokens whereas nsc uses CASECLASS
    val isTrait: Boolean = TRAIT
    nextToken()
    ident()
    typeParamClauseOpt()
    annotations(skipNewLines = false, requireOneArgList = true)
    if (!isTrait) {
      accessModifierOpt()
      paramClauses()
    }
    templateOpt(isTrait)
  }

  private def objectDef() = {
    if (CASE) nextToken() // We use two tokens whereas nsc uses CASEOBJECT
    nextToken()
    ident()
    templateOpt(isTrait = false)
  }

  private def templateParents(isTrait: Boolean) {
    annotType(isPattern = false)
    if (LPAREN && !isTrait) {
      do {
        argumentExprs()
      } while (LPAREN)
    }
    while (WITH) {
      nextToken()
      annotType(isPattern = false)
    }
  }

  private def template(isTrait: Boolean) {

    newLineOptWhenFollowedBy(LBRACE)
    if (LBRACE) {
      templateBody()
      if (WITH) { // TODO check cond
        nextToken()
        templateParents(isTrait)
        templateBodyOpt()
      }

    } else {
      templateParents(isTrait)
      templateBodyOpt()
    }

  }

  private def templateOpt(isTrait: Boolean) = {
    if (EXTENDS) {
      nextToken()
      template(isTrait)
    } else if (SUBTYPE && isTrait) {
      nextToken()
      template(isTrait = true)
    } else {
      newLineOptWhenFollowedBy(LBRACE)
      templateBodyOpt()
    }

  }

  private def templateBody() = {
    accept(LBRACE)
    templateStatSeq()
    accept(RBRACE)
  }

  private def templateBodyOpt() = {
    newLineOptWhenFollowedBy(LBRACE)
    if (LBRACE)
      templateBody()
    else if (LPAREN)
      throw new ScalaParserException("traits or objects may not have parametsrs")
  }

  private def refinement() = {
    accept(LBRACE)
    refineStatSeq()
    accept(RBRACE)
  }

  private def packaging() = {
    qualId()
    newLineOptWhenFollowedBy(LBRACE)
    accept(LBRACE)
    topStatSeq()
    accept(RBRACE)
  }

  private def topStatSeq() {

    while (!RBRACE && !EOF) {

      if (PACKAGE) {
        nextToken()
        if (OBJECT) {
          objectDef()
        } else {
          packaging()
        }

      } else if (IMPORT) {
        importClause()
      } else if (CLASS || caseClass || TRAIT || OBJECT || caseObject || AT || isModifier) {
        topLevelTmplDef()
      } else if (!isStatSep)
        throw new ScalaParserException("expected class or object definition")
      if (!RBRACE && !EOF) acceptStatSep()
    }

  }

  private def templateStatSeq() = {

    if (isExprIntro) {

      expr(InTemplate)
      if (ARROW) {
        nextToken()
      } else {
        if (!RBRACE && !EOF) acceptStatSep()
      }
    }
    while (!RBRACE && !EOF) {

      if (IMPORT)
        importClause()
      else if (isExprIntro)
        statement(InTemplate)
      else if (isDefIntro || isModifier || AT)
        nonLocalDefOrDcl()
      else if (!isStatSep)
        throw new ScalaParserException("illegal start of definition: " + currentToken)
      if (!RBRACE && !EOF) acceptStatSep()
    }

  }

  private def refineStatSeq() = {
    while (!RBRACE && !EOF) {
      if (isDclIntro)
        defOrDcl()
      else if (!isStatSep)
        throw new ScalaParserException("illegal start of definition: " + currentToken)
      if (!RBRACE) acceptStatSep()
    }
  }
  private def localDef() = {
    annotations(skipNewLines = true, requireOneArgList = false)
    localModifiers()
    // val modifierCondition = true // TODO: !!!!

    or(defOrDcl(), tmplDef())
  }

  private def blockStatSeq() = {
    while (!RBRACE && !EOF && !justCase) {
      if (IMPORT) {
        importClause()
        acceptStatSep()
      } else if (isExprIntro) {
        statement(InBlock)
        if (!RBRACE && !justCase) acceptStatSep()
      } else if (isDefIntro || isLocalModifier || AT) {
        if (IMPLICIT) {
          nextToken()
          if (isIdent)
            implicitClosure(InBlock)
          else
            localDef()
        } else
          localDef()
        if (!RBRACE && !justCase) acceptStatSep()
      } else if (isStatSep)
        nextToken()
      else
        throw new ScalaParserException("illegal start of statement: " + currentToken)
    }

  }

  def compilationUnit() = {
    def topstats() {
      while (SEMI) nextToken()
      if (PACKAGE) {
        nextToken()
        if (OBJECT) {
          objectDef()
          if (!EOF) {
            acceptStatSep()
            topStatSeq()
          }
        } else {
          qualId()
          newLineOptWhenFollowedBy(LBRACE)
          if (EOF)
            ()
          else if (isStatSep) {
            nextToken()
            topstats()
          } else {
            accept(LBRACE)
            topStatSeq()
            accept(RBRACE)
            topStatSeq()
          }
        }
      } else
        topStatSeq()
    }

    topstats()

  }

  private def xmlStartTag(isPattern: Boolean) {
    accept(XML_START_OPEN)
    accept(XML_NAME)
    while (!XML_TAG_CLOSE) {
      if (XML_WHITESPACE)
        nextToken()
      currentTokenType match {
        case XML_NAME =>
          xmlAttribute(isPattern)
        case XML_TAG_CLOSE =>
        // End loop
        case _ =>
          throw new ScalaParserException("Expected XML attribute or end of tag: " + currentToken)
      }
    }
    accept(XML_TAG_CLOSE)
  }

  private def xmlAttribute(isPattern: Boolean) {
    accept(XML_NAME)
    if (XML_WHITESPACE)
      nextToken()
    accept(XML_ATTR_EQ)
    if (XML_WHITESPACE)
      nextToken()
    currentTokenType match {
      case XML_ATTR_VALUE =>
        nextToken()
      case LBRACE =>
        xmlEmbeddedScala(isPattern)
    }
  }

  private def xmlEmptyElement(isPattern: Boolean) {
    accept(XML_START_OPEN)
    accept(XML_NAME)
    while (!XML_EMPTY_CLOSE) {
      if (XML_WHITESPACE)
        nextToken()
      currentTokenType match {
        case XML_NAME =>
          xmlAttribute(isPattern)
        case XML_EMPTY_CLOSE =>
        // End loop
        case _ =>
          throw new ScalaParserException("Expected XML attribute or end of tag: " + currentToken)
      }
    }
    accept(XML_EMPTY_CLOSE)
  }

  private def xmlEmbeddedScala(isPattern: Boolean) = {
    if (isPattern) {
      accept(LBRACE)
      patterns(seqOK = true)
      accept(RBRACE)
    } else {
      blockExpr()
    }

  }

  private def xmlEndTag() {
    accept(XML_END_OPEN)
    accept(XML_NAME)
    if (XML_WHITESPACE)
      nextToken()
    accept(XML_TAG_CLOSE)
  }

  private def xmlNonEmptyElement(isPattern: Boolean) {
    xmlStartTag(isPattern)
    while (!XML_END_OPEN) {
      currentTokenType match {
        case XML_START_OPEN =>
          xmlElement(isPattern)
        case XML_PCDATA | XML_COMMENT | XML_CDATA | XML_UNPARSED | XML_PROCESSING_INSTRUCTION =>
          nextToken()
        case LBRACE =>
          xmlEmbeddedScala(isPattern)
        case _ => 
          throw new ScalaParserException("Unexpected token in XML: " + currentToken)
      }
    }
    xmlEndTag()
  }

  private def xmlElement(isPattern: Boolean) = {
    or(xmlNonEmptyElement(isPattern), xmlEmptyElement(isPattern))
  }

  private def xml(isPattern: Boolean) {
    def xmlContent() {
      currentTokenType match {
        case XML_COMMENT | XML_CDATA | XML_UNPARSED | XML_PROCESSING_INSTRUCTION | XML_PCDATA =>
          nextToken()
        case XML_START_OPEN =>
          xmlElement(isPattern)
        case _ => throw new ScalaParserException("Expected XML: " + currentToken)
      }
    }
    xmlContent()
    while (XML_START_OPEN || XML_PCDATA) {
      if (XML_START_OPEN) 
        xmlElement(isPattern)
      else
        accept(XML_PCDATA)
    }
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

  private def optional[T](p: => T): Option[T] =
    or(Some(p), None)

  private def or[T](p1: => T, p2: => T): T = {
    val originalPos = pos
    try {
      p1
    } catch {
      case e: ScalaParserException =>
        pos = originalPos
        p2
    }
  }

}
