package scalariform.formatter

import scalariform.lexer.Token
import scalariform.lexer.Tokens._
import scalariform.parser._
import scalariform.utils.Utils
import scalariform.formatter.preferences._
import PartialFunction._

trait ExprFormatter { self: HasFormattingPreferences with AnnotationFormatter with HasHiddenTokenInfo with TypeFormatter with TemplateFormatter with ScalaFormatter with XmlFormatter with CaseClauseFormatter ⇒

  def format(expr: Expr)(implicit formatterState: FormatterState): FormatResult = format(expr.contents)

  private def format(exprElements: List[ExprElement])(implicit formatterState: FormatterState): FormatResult = formatExprElements(exprElements)._1

  private def format(exprElement: ExprElement)(implicit formatterState: FormatterState): FormatResult = exprElement match {
    case ifExpr: IfExpr                       ⇒ format(ifExpr)
    case whileExpr: WhileExpr                 ⇒ format(whileExpr)
    case matchExpr: MatchExpr                 ⇒ format(matchExpr)
    case doExpr: DoExpr                       ⇒ format(doExpr)
    case blockExpr: BlockExpr                 ⇒ format(blockExpr, indent = true)
    case forExpr: ForExpr                     ⇒ format(forExpr)
    case tryExpr: TryExpr                     ⇒ format(tryExpr)
    case template: Template                   ⇒ format(template)
    case statSeq: StatSeq                     ⇒ format(statSeq) // TODO: revisit
    case argumentExprs: ArgumentExprs         ⇒ format(argumentExprs)._1
    case anonymousFunction: AnonymousFunction ⇒ format(anonymousFunction)
    case GeneralTokens(_)                     ⇒ NoFormatResult
    case PrefixExprElement(_)                 ⇒ NoFormatResult
    case infixExpr: InfixExpr                 ⇒ format(infixExpr)._1
    case postfixExpr: PostfixExpr             ⇒ format(postfixExpr)
    case annotation: Annotation               ⇒ format(annotation)
    case typeExprElement: TypeExprElement     ⇒ format(typeExprElement.contents)
    case expr: Expr                           ⇒ format(expr.contents)
    case argument: Argument                   ⇒ format(argument.expr)
    case xmlExpr: XmlExpr                     ⇒ format(xmlExpr)
    case parenExpr: ParenExpr                 ⇒ format(parenExpr)._1
    case new_ : New                           ⇒ format(new_.template)
    case callExpr: CallExpr                   ⇒ format(callExpr)._1
    case equalsExpr: EqualsExpr               ⇒ format(equalsExpr)
    case ascriptionExpr: AscriptionExpr       ⇒ format(ascriptionExpr)
    case _                                    ⇒ NoFormatResult
  }

  private def format(ascriptionExpr: AscriptionExpr)(implicit formatterState: FormatterState): FormatResult = {
    val AscriptionExpr(left: List[ExprElement], colon: Token, right: List[ExprElement]) = ascriptionExpr
    var formatResult: FormatResult = NoFormatResult
    var currentFormatterState = formatterState
    val (leftFormatResult, updatedFormatterState) = formatExprElements(left)
    currentFormatterState = updatedFormatterState
    formatResult ++= leftFormatResult
    formatResult ++= format(right)(currentFormatterState)
    formatResult
  }

  private def format(equalsExpr: EqualsExpr)(implicit formatterState: FormatterState): FormatResult = {
    val EqualsExpr(lhs: List[ExprElement], equals: Token, rhs: Expr) = equalsExpr
    var formatResult: FormatResult = NoFormatResult
    var currentFormatterState = formatterState
    val (leftFormatResult, updatedFormatterState) = formatExprElements(lhs)
    currentFormatterState = updatedFormatterState
    formatResult ++= leftFormatResult
    if (hiddenPredecessors(rhs.firstToken).containsNewline) {
      currentFormatterState = currentFormatterState.indent
      formatResult = formatResult.before(rhs.firstToken, currentFormatterState.currentIndentLevelInstruction)
    }
    formatResult ++= format(rhs)(currentFormatterState)
    formatResult
  }

  private def format(matchExpr: MatchExpr)(implicit formatterState: FormatterState): FormatResult = {
    val MatchExpr(left: List[ExprElement], matchToken: Token, block: BlockExpr) = matchExpr
    var formatResult: FormatResult = NoFormatResult
    var currentFormatterState = formatterState
    val (leftFormatResult, updatedFormatterState) = formatExprElements(left)
    currentFormatterState = updatedFormatterState
    formatResult ++= leftFormatResult
    formatResult ++= format(block)(currentFormatterState)
    formatResult
  }

  private def formatExprElements(exprElements: List[ExprElement])(implicit formatterState: FormatterState): (FormatResult, FormatterState) = {
    if (exprElements flatMap { _.tokens } isEmpty)
      return (NoFormatResult, formatterState)
    var formatResult: FormatResult = NoFormatResult
    var currentFormatterState = formatterState

    for ((previousElementOption, element, nextElementOption) ← Utils.withPreviousAndNext(exprElements)) {
      previousElementOption match {
        case Some(previousElement) ⇒
          var nestedFormatterState = currentFormatterState
          val instructionOption = condOpt(previousElement, element) {
            case (PrefixExprElement(_), _) ⇒ Compact
            case (Argument(_), _) ⇒ Compact
            case (_, _: ArgumentExprs) if formattingPreferences(PreserveSpaceBeforeArguments) ⇒ CompactPreservingGap // TODO: Probably not needed now with CallExpr
            case (_, _) if element.firstTokenOption exists { firstToken ⇒ newlineBefore(firstToken) && !(Set(COMMA, COLON) contains firstToken.getType) } ⇒
              currentFormatterState = currentFormatterState.indentForExpressionBreakIfNeeded
              currentFormatterState.currentIndentLevelInstruction
          }

          for (instruction ← instructionOption if !element.firstToken.isNewline)
            formatResult = formatResult.before(element.firstToken, instruction)
        case None ⇒
      }

      val firstToken = exprElements.head.tokens.head
      element match {
        case infixExpr @ InfixExpr(_, _, _, _) ⇒
          val (extraFormatResult, newFormatterState) = format(infixExpr)(currentFormatterState)
          formatResult ++= extraFormatResult
          currentFormatterState = newFormatterState
        case callExpr: CallExpr ⇒
          val (extraFormatResult, newFormatterState) = format(callExpr)(currentFormatterState)
          formatResult ++= extraFormatResult
          currentFormatterState = newFormatterState
        case parenExpr: ParenExpr ⇒
          val expressionBreakHappened = currentFormatterState.expressionBreakHappened
          val (extraFormatResult, newFormatterState) = format(parenExpr)(currentFormatterState.clearExpressionBreakHappened)
          formatResult ++= extraFormatResult
          currentFormatterState = newFormatterState.copy(
            expressionBreakHappened = expressionBreakHappened || newFormatterState.expressionBreakHappened)
        case GeneralTokens(_) | PrefixExprElement(_) ⇒
          for (token ← element.tokens if token != firstToken)
            if (isInferredNewline(token))
              nextElementOption match {
                case Some(_: ArgumentExprs) if token == element.tokens.last ⇒
                  () //  We don't do expression breaks immediately before an argument expression
                case _ ⇒
                  currentFormatterState = currentFormatterState.indentForExpressionBreakIfNeeded
                  formatResult = formatResult.formatNewline(token, currentFormatterState.currentIndentLevelInstruction)
              }
            else if (hiddenPredecessors(token).containsNewline && !(Set(COMMA, COLON) contains token.getType)) { // TODO: Probably not needed now, see above
              currentFormatterState = currentFormatterState.indentForExpressionBreakIfNeeded
              formatResult = formatResult.before(token, currentFormatterState.currentIndentLevelInstruction)
            }
        case _ ⇒
          formatResult ++= format(element)(currentFormatterState.clearExpressionBreakHappened)
      }
    }

    (formatResult, currentFormatterState)
  }

  private def format(callExpr: CallExpr)(implicit initialFormatterState: FormatterState): (FormatResult, FormatterState) = {
    val CallExpr(exprDotOpt: Option[(List[ExprElement], Token)], id, typeArgsOpt: Option[TypeExprElement], newLineOptsAndArgumentExprss: List[(Option[Token], ArgumentExprs)], uscoreOpt) = callExpr
    var formatResult: FormatResult = NoFormatResult
    var currentFormatterState = initialFormatterState

    for ((left, dot) ← exprDotOpt) {
      val (leftResult, leftUpdatedFormatterState) = formatExprElements(left)
      currentFormatterState = leftUpdatedFormatterState
      formatResult ++= leftResult

      if (hiddenPredecessors(dot).containsNewline) {
        currentFormatterState = initialFormatterState.indentForExpressionBreakIfNeeded
        formatResult = formatResult.before(dot, currentFormatterState.currentIndentLevelInstruction)
      }
    }

    if (exprDotOpt.isDefined && hiddenPredecessors(id).containsNewline) {
      currentFormatterState = currentFormatterState.indentForExpressionBreakIfNeeded
      formatResult = formatResult.before(id, currentFormatterState.currentIndentLevelInstruction)
    }

    for (typeArgs ← typeArgsOpt)
      formatResult ++= format(typeArgs)(currentFormatterState.clearExpressionBreakHappened)

    for ((newlineOpt, argumentExprs) ← newLineOptsAndArgumentExprss) {
      if (newlineOpt == None && formattingPreferences(PreserveSpaceBeforeArguments))
        formatResult = formatResult.before(argumentExprs.firstToken, CompactPreservingGap)
      val expressionBreakHappened = currentFormatterState.expressionBreakHappened
      val (argResult, argUpdatedFormatterState) = format(argumentExprs)(currentFormatterState.clearExpressionBreakHappened)
      currentFormatterState = argUpdatedFormatterState.copy(
        expressionBreakHappened = expressionBreakHappened || argUpdatedFormatterState.expressionBreakHappened)
      formatResult ++= argResult
    }

    for (uscore ← uscoreOpt)
      formatResult = formatResult.before(uscore, CompactPreservingGap)

    (formatResult, currentFormatterState)
  }

  private def format(infixExpr: InfixExpr)(implicit initialFormatterState: FormatterState): (FormatResult, FormatterState) = {
    val InfixExpr(left: List[ExprElement], infixId: Token, newlineOption: Option[Token], right: List[ExprElement]) = infixExpr
    var formatResult: FormatResult = NoFormatResult

    var currentFormatterState = initialFormatterState

    val (leftResult, updatedFormatterState) = formatExprElements(left)(currentFormatterState)
    formatResult ++= leftResult
    currentFormatterState = updatedFormatterState

    val firstRightToken = right.head.firstToken
    val lastLeftToken = left.last.lastToken
    val stringLiteral = firstRightToken.tokenType == STRING_LITERAL || lastLeftToken.tokenType == STRING_LITERAL
    val isStringConcatenation = infixId.tokenType == PLUS && formattingPreferences(CompactStringConcatenation) && stringLiteral

    val (beforeInfixInstruction, updatedFormatterState2) =
      if (isStringConcatenation)
        (Compact, currentFormatterState)
      else if (hiddenPredecessors(infixId).containsNewline) {
        val newFormatterState = currentFormatterState.indentForExpressionBreakIfNeeded
        (newFormatterState.currentIndentLevelInstruction, newFormatterState)
      } else
        (CompactEnsuringGap, currentFormatterState)
    formatResult = formatResult.before(infixId, beforeInfixInstruction)
    currentFormatterState = updatedFormatterState2

    currentFormatterState = newlineOption match {
      case Some(newline) ⇒
        val newFormatterState = currentFormatterState.indentForExpressionBreakIfNeeded
        formatResult = formatResult.formatNewline(newline, newFormatterState.currentIndentLevelInstruction)
        newFormatterState
      case None if hiddenPredecessors(firstRightToken).containsNewline ⇒
        val newFormatterState = currentFormatterState.indentForExpressionBreakIfNeeded
        formatResult = formatResult.before(firstRightToken, newFormatterState.currentIndentLevelInstruction)
        newFormatterState
      case _ ⇒
        formatResult = formatResult.before(firstRightToken, if (isStringConcatenation) Compact else CompactEnsuringGap)
        currentFormatterState
    }

    val (rightResult, rightUpdatedFormatterState) = formatExprElements(right)(currentFormatterState)
    formatResult ++= rightResult
    currentFormatterState = rightUpdatedFormatterState

    (formatResult, currentFormatterState)
  }

  private def format(postfixExpr: PostfixExpr)(implicit formatterState: FormatterState): FormatResult = {
    val PostfixExpr(first: List[ExprElement], postfixId: Token) = postfixExpr
    var formatResult: FormatResult = NoFormatResult
    formatResult ++= format(first)
    formatResult = formatResult.before(postfixId, CompactPreservingGap)
    formatResult
  }

  def format(anonymousFunction: AnonymousFunction)(implicit formatterState: FormatterState): FormatResult = { // <-- Also formatted specially in BlockExpr
    val AnonymousFunction(parameters, arrow, body) = anonymousFunction
    var formatResult: FormatResult = NoFormatResult
    formatResult ++= format(parameters)
    val bodyFirstTokenOpt = body.tokens.headOption
    val newlineBeforeBody = bodyFirstTokenOpt exists { hiddenPredecessors(_).containsNewline }
    if (newlineBeforeBody) {
      formatResult = formatResult.before(bodyFirstTokenOpt.get, formatterState.nextIndentLevelInstruction)
      formatResult ++= format(body)(formatterState.indent)
    } else
      formatResult ++= format(body)(formatterState)
    formatResult
  }

  def format(argumentExprs: ArgumentExprs)(implicit formatterState: FormatterState): (FormatResult, FormatterState) = argumentExprs match {
    case BlockArgumentExprs(contents) ⇒ (format(contents), formatterState)
    case ParenArgumentExprs(lparen, contents, rparen) ⇒
      var currentFormatterState = formatterState
      var formatResult: FormatResult = NoFormatResult
      val (contentsFormatResult, updatedFormatterState) = formatExprElements(GeneralTokens(List(lparen)) :: contents)
      formatResult ++= contentsFormatResult
      currentFormatterState = updatedFormatterState
      if (formattingPreferences(PreserveDanglingCloseParenthesis) && hiddenPredecessors(rparen).containsNewline && contents.nonEmpty)
        formatResult = formatResult.before(rparen, formatterState.currentIndentLevelInstruction)
      (formatResult, currentFormatterState)
  }

  private def format(parenExpr: ParenExpr)(implicit formatterState: FormatterState): (FormatResult, FormatterState) = {
    val ParenExpr(lparen, contents, rparen) = parenExpr
    format(ParenArgumentExprs(lparen, contents, rparen))
  }

  private def format(tryExpr: TryExpr)(implicit formatterState: FormatterState): FormatResult = {
    val TryExpr(tryToken: Token, body: Expr, catchClauseOption: Option[CatchClause], finallyClauseOption: Option[(Token, Expr)]) = tryExpr
    var formatResult: FormatResult = NoFormatResult

    // TODO: similar to first half of ifExpr, whileExpr etc

    val bodyIsABlock = isBlockExpr(body)

    val indentBody =
      if (hiddenPredecessors(body.firstToken).containsNewline) {
        if (bodyIsABlock) {
          formatResult = formatResult.before(body.firstToken, CompactEnsuringGap)
          false
        } else {
          formatResult = formatResult.before(body.firstToken, formatterState.nextIndentLevelInstruction)
          true
        }
      } else {
        formatResult = formatResult.before(body.firstToken, CompactEnsuringGap)
        false
      }
    val bodyFormatterState = if (indentBody) formatterState.indent else formatterState
    formatResult ++= format(body)(bodyFormatterState)

    // TODO: Simplified version of elseClause formatting
    for (CatchClause(catchToken, catchBlockOrExpr) ← catchClauseOption) {
      if (formattingPreferences(CompactControlReadability) && bodyIsABlock && containsNewline(body))
        formatResult = formatResult.before(catchToken, formatterState.currentIndentLevelInstruction)
      else if (hiddenPredecessors(catchToken).containsNewline && !(bodyIsABlock && containsNewline(body)))
        formatResult = formatResult.before(catchToken, formatterState.currentIndentLevelInstruction)

      catchBlockOrExpr match {
        case Left(catchBlock) ⇒
          formatResult = formatResult.before(catchBlock.firstToken, CompactEnsuringGap)
          formatResult ++= format(catchBlock)
        case Right(catchExpr) ⇒
          val indentCatchExpr = hiddenPredecessors(catchExpr.firstToken).containsNewline
          val instruction = if (indentCatchExpr) formatterState.nextIndentLevelInstruction else CompactEnsuringGap
          formatResult = formatResult.before(catchExpr.firstToken, instruction)
          val catchExprFormatterState = if (indentCatchExpr) formatterState.indent else formatterState
          formatResult ++= format(catchExpr)(catchExprFormatterState)
      }
    }

    // TODO: See elseClause formatting
    for ((finallyToken, finallyBody) ← finallyClauseOption) {

      val previousIsMultilineBracesBlock = catchClauseOption.isEmpty && bodyIsABlock && containsNewline(body) ||
        cond(catchClauseOption) { case Some(CatchClause(_, Left(catchBlock))) ⇒ containsNewline(catchBlock) }

      if (formattingPreferences(CompactControlReadability) && previousIsMultilineBracesBlock)
        formatResult = formatResult.before(finallyToken, formatterState.currentIndentLevelInstruction)
      else if (hiddenPredecessors(finallyToken).containsNewline && !previousIsMultilineBracesBlock)
        formatResult = formatResult.before(finallyToken, formatterState.currentIndentLevelInstruction)

      val indentFinallyBody =
        if (isBlockExpr(finallyBody))
          false
        else if (hiddenPredecessors(finallyBody.firstToken).containsNewline)
          true
        else
          false

      if (indentFinallyBody)
        formatResult = formatResult.before(finallyBody.firstToken, formatterState.nextIndentLevelInstruction)
      else
        formatResult = formatResult.before(finallyBody.firstToken, CompactEnsuringGap)

      val finallyBodyFormatterState = if (indentFinallyBody) formatterState.indent else formatterState
      formatResult ++= format(finallyBody)(finallyBodyFormatterState)
    }

    formatResult
  }

  private def format(ifExpr: IfExpr)(implicit formatterState: FormatterState): FormatResult = {
    val IfExpr(ifToken: Token, condExpr: CondExpr, newlinesOpt: Option[Token], body: Expr, elseClauseOption: Option[ElseClause]) = ifExpr
    var formatResult: FormatResult = NoFormatResult

    // TODO: Same as first half of whileExpr

    formatResult ++= format(condExpr)

    val bodyIsABlock = isBlockExpr(body)

    val indentBody = newlinesOpt match {
      case Some(newlines) if bodyIsABlock ⇒
        formatResult = formatResult.formatNewline(newlines, CompactEnsuringGap)
        false
      case Some(newlines) ⇒
        formatResult = formatResult.formatNewline(newlines, formatterState.nextIndentLevelInstruction)
        true
      case None if hiddenPredecessors(body.firstToken).containsNewline && !bodyIsABlock ⇒
        formatResult = formatResult.before(body.firstToken, formatterState.nextIndentLevelInstruction)
        false
      case None ⇒
        formatResult = formatResult.before(body.firstToken, CompactEnsuringGap)
        false
    }

    val bodyFormatterState = if (indentBody) formatterState.indent else formatterState

    formatResult ++= format(body)(bodyFormatterState)

    // TODO: take into account pre-Else semi
    for (ElseClause(elseSemiOpt, elseToken, elseBody) ← elseClauseOption) {
      if (formattingPreferences(CompactControlReadability) && bodyIsABlock && containsNewline(body))
        formatResult = formatResult.before(elseToken, formatterState.currentIndentLevelInstruction)
      else if (bodyIsABlock && containsNewline(body))
        formatResult = formatResult.before(elseToken, CompactEnsuringGap)
      else if (hiddenPredecessors(elseToken).containsNewline || containsNewline(body) || (indentBody && (hiddenPredecessors(elseBody.firstToken).containsNewline || isBlockExpr(elseBody) || isIfExpr(elseBody))))
        formatResult = formatResult.before(elseToken, formatterState.currentIndentLevelInstruction)

      val indentElseBody =
        if (isBlockExpr(elseBody) || isIfExpr(elseBody))
          false
        else if (hiddenPredecessors(elseBody.firstToken).containsNewline)
          true
        else
          false

      if (indentElseBody)
        formatResult = formatResult.before(elseBody.firstToken, formatterState.nextIndentLevelInstruction)
      else
        formatResult = formatResult.before(elseBody.firstToken, CompactEnsuringGap)

      val elseBodyFormatterState = if (indentElseBody) formatterState.indent else formatterState
      formatResult ++= format(elseBody)(elseBodyFormatterState)
    }

    formatResult
  }

  private def format(condExpr: CondExpr)(implicit formatterState: FormatterState): FormatResult = {
    val CondExpr(lparen: Token, condition: Expr, rparen: Token) = condExpr
    format(condition)
  }

  private def isIfExpr(expr: Expr) = expr.contents.size == 1 && expr.contents(0).isInstanceOf[IfExpr]

  private def format(forExpr: ForExpr)(implicit formatterState: FormatterState): FormatResult = {
    val ForExpr(
      forToken: Token,
      lParenOrBrace: Token,
      enumerators: Enumerators,
      rParenOrBrace: Token,
      newlinesOpt: Option[Token],
      yieldOption: Option[Token],
      body: Expr) = forExpr
    var formatResult: FormatResult = NoFormatResult

    // TODO: similar to blockExpr
    val enumeratorsSectionContainsNewline = containsNewline(enumerators) ||
      hiddenPredecessors(rParenOrBrace).containsNewline ||
      hiddenPredecessors(enumerators.firstToken).containsNewline
    if (enumeratorsSectionContainsNewline) {
      formatResult = formatResult.before(enumerators.firstToken, formatterState.nextIndentLevelInstruction)
      formatResult ++= format(enumerators)(formatterState.indent)
      formatResult = formatResult.before(rParenOrBrace, formatterState.currentIndentLevelInstruction)
    } else
      formatResult ++= format(enumerators)(formatterState)

    // TODO: similar to whileExpr, first half of ifExpr
    val bodyIsABlock = isBlockExpr(body)

    val ensureNoNewline = bodyIsABlock || enumeratorsSectionContainsNewline
    val indentBody = newlinesOpt match {
      case Some(newlines) if ensureNoNewline ⇒
        formatResult = formatResult.formatNewline(newlines, CompactEnsuringGap)
        false
      case Some(newlines) ⇒
        formatResult = formatResult.formatNewline(newlines, formatterState.nextIndentLevelInstruction)
        true
      case None if (yieldOption exists { hiddenPredecessors(_).containsNewline }) && !ensureNoNewline ⇒
        formatResult = formatResult.before(yieldOption.get, formatterState.nextIndentLevelInstruction)
        false
      case None if yieldOption.isEmpty && hiddenPredecessors(body.firstToken).containsNewline && !ensureNoNewline ⇒
        formatResult = formatResult.before(body.firstToken, formatterState.nextIndentLevelInstruction)
        false
      case None ⇒
        formatResult = formatResult.before(body.firstToken, CompactEnsuringGap)
        false
    }

    val bodyFormatterState = if (indentBody) formatterState.indent else formatterState
    formatResult ++= format(body)(bodyFormatterState)

    formatResult
  }

  private def format(enumerators: Enumerators)(implicit formatterState: FormatterState): FormatResult = {
    val Enumerators(initialGenerator: Generator, rest: List[(Token, Enumerator)]) = enumerators
    var formatResult: FormatResult = NoFormatResult

    formatResult ++= format(initialGenerator)
    // TODO: Pretty similar to statSeq
    for ((semi, otherEnumerator) ← rest) {

      if (isInferredNewline(semi))
        formatResult = formatResult.formatNewline(semi, formatterState.currentIndentLevelInstruction)

      if (!isInferredNewline(semi)) {
        val firstToken = otherEnumerator.firstToken
        val instruction = if (hiddenPredecessors(firstToken).containsNewline)
          formatterState.currentIndentLevelInstruction
        else
          CompactEnsuringGap
        formatResult = formatResult.before(firstToken, instruction)
      }
      formatResult ++= format(otherEnumerator)
    }

    formatResult
  }

  private def format(enumerator: Enumerator)(implicit formatterState: FormatterState): FormatResult = {
    enumerator match {
      case expr @ Expr(_)                       ⇒ format(expr)
      case generator @ Generator(_, _, _, _, _) ⇒ format(generator)
      case guard @ Guard(_, _)                  ⇒ format(guard: Guard)
    }
  }

  private def format(generator: Generator)(implicit formatterState: FormatterState): FormatResult = {
    val Generator(valOption: Option[Token], pattern: Expr, equalsOrArrowToken: Token, expr: Expr, guards: List[Guard]) = generator
    var formatResult: FormatResult = NoFormatResult
    formatResult ++= format(expr)
    formatResult ++= format(pattern)
    for (guard ← guards)
      formatResult ++= format(guard)
    formatResult
  }

  def format(guard: Guard)(implicit formatterState: FormatterState): FormatResult = {
    val Guard(ifToken: Token, expr: Expr) = guard
    format(expr)
  }

  private def format(whileExpr: WhileExpr)(implicit formatterState: FormatterState): FormatResult = {
    // TODO: Same as first half of ifExpr
    val WhileExpr(whileToken: Token, condExpr: CondExpr, newlinesOpt: Option[Token], body: Expr) = whileExpr
    var formatResult: FormatResult = NoFormatResult

    formatResult ++= format(condExpr)

    val bodyIsABlock = isBlockExpr(body)

    val indentBody = newlinesOpt match {
      case Some(newlines) if bodyIsABlock ⇒
        formatResult = formatResult.formatNewline(newlines, CompactEnsuringGap)
        false
      case Some(newlines) ⇒
        formatResult = formatResult.formatNewline(newlines, formatterState.nextIndentLevelInstruction)
        true
      case None if hiddenPredecessors(body.firstToken).containsNewline && !bodyIsABlock ⇒
        formatResult = formatResult.before(body.firstToken, formatterState.nextIndentLevelInstruction)
        false
      case None ⇒
        formatResult = formatResult.before(body.firstToken, CompactEnsuringGap)
        false
    }

    val bodyFormatterState = if (indentBody) formatterState.indent else formatterState
    formatResult ++= format(body)(bodyFormatterState)

    formatResult
  }

  private def format(doExpr: DoExpr)(implicit formatterState: FormatterState): FormatResult = {
    var formatResult: FormatResult = NoFormatResult
    val DoExpr(doToken: Token, body: Expr, statSepOpt: Option[Token], whileToken: Token, condExpr: CondExpr) = doExpr

    val bodyIsABlock = isBlockExpr(body)
    val indentBody = !bodyIsABlock && hiddenPredecessors(body.firstToken).containsNewline
    val instruction =
      if (indentBody) formatterState.nextIndentLevelInstruction
      else CompactEnsuringGap
    formatResult = formatResult.before(body.firstToken, instruction)

    val bodyFormatterState = if (indentBody) formatterState.indent else formatterState
    formatResult ++= format(body)(bodyFormatterState)

    formatResult = statSepOpt match {
      case Some(semi) if isInferredNewline(semi) ⇒ {
        val instruction =
          if (indentBody)
            formatterState.currentIndentLevelInstruction
          else if (bodyIsABlock && containsNewline(body))
            CompactEnsuringGap
          else
            formatterState.currentIndentLevelInstruction
        formatResult.formatNewline(semi, instruction)
      }
      case _ ⇒ {
        val instruction =
          if (indentBody)
            formatterState.currentIndentLevelInstruction
          else if (bodyIsABlock && containsNewline(body))
            CompactEnsuringGap
          else if (hiddenPredecessors(whileToken).containsNewline)
            formatterState.currentIndentLevelInstruction
          else
            CompactEnsuringGap
        formatResult.before(whileToken, instruction)
      }
    }

    formatResult ++= format(condExpr)

    formatResult
  }

  private def isBlockExpr(expr: Expr) = expr.contents.size == 1 && expr.contents(0).isInstanceOf[BlockExpr]

  def format(blockExpr: BlockExpr, indent: Boolean)(implicit formatterState: FormatterState): FormatResult = {
    val BlockExpr(lbrace: Token, caseClausesOrStatSeq: Either[CaseClauses, StatSeq], rbrace: Token) = blockExpr
    var formatResult: FormatResult = NoFormatResult
    val singleLineBlock = !containsNewline(blockExpr)
    val newFormatterState = formatterState.copy(inSingleLineBlock = singleLineBlock)
    val (indentedInstruction, indentedState) =
      if (indent)
        (newFormatterState.nextIndentLevelInstruction, newFormatterState.indent)
      else
        (newFormatterState.currentIndentLevelInstruction, newFormatterState)
    caseClausesOrStatSeq match {
      case Left(caseClauses) ⇒ // TODO: Duplication
        if (!singleLineBlock) {
          formatResult = formatResult.before(caseClauses.firstToken, indentedInstruction)
          formatResult ++= format(caseClauses)(indentedState)
          formatResult = formatResult.before(rbrace, newFormatterState.currentIndentLevelInstruction)
        } else
          formatResult ++= format(caseClauses)(newFormatterState)

      case Right(statSeq) ⇒
        if (singleLineBlock)
          formatResult ++= format(statSeq)(newFormatterState)
        else {
          if (statSeq.firstTokenOption.isDefined) {
            statSeq.firstStatOpt match {
              case Some(Expr(List(anonFn @ AnonymousFunction(params, arrowToken, subStatSeq)))) ⇒
                val (instruction, subStatState) =
                  if (hiddenPredecessors(params(0).firstToken).containsNewline)
                    (indentedInstruction, indentedState.indent)
                  else
                    (CompactEnsuringGap, indentedState)
                formatResult = formatResult.before(statSeq.firstToken, instruction)
                formatResult ++= format(params)
                for (firstToken ← subStatSeq.firstTokenOption) {
                  val instruction =
                    if (hiddenPredecessors(firstToken).containsNewline || containsNewline(subStatSeq))
                      statFormatterState(subStatSeq.firstStatOpt)(subStatState).currentIndentLevelInstruction
                    else
                      CompactEnsuringGap
                  formatResult = formatResult.before(firstToken, instruction)
                }
                formatResult ++= format(subStatSeq)(subStatState)
              case _ ⇒
                val instruction = statSeq.selfReferenceOpt match {
                  case Some((selfReference, arrow)) if !hiddenPredecessors(selfReference.firstToken).containsNewline ⇒
                    CompactEnsuringGap
                  case _ ⇒
                    statFormatterState(statSeq.firstStatOpt)(indentedState).currentIndentLevelInstruction
                }
                formatResult = formatResult.before(statSeq.firstToken, instruction)
                formatResult ++= format(statSeq)(indentedState)
            }
          }
          formatResult = formatResult.before(rbrace, newFormatterState.currentIndentLevelInstruction)
        }
    }

    formatResult
  }

  private def statFormatterState(statOpt: Option[Stat])(implicit formatterState: FormatterState) = statOpt match {
    case Some(FullDefOrDcl(_, _, FunDefOrDcl(_, _, _, _, _, _, /* localDef = */ true))) if formattingPreferences(IndentLocalDefs) ⇒
      formatterState.indent
    case _ ⇒
      formatterState
  }

  def format(statSeq: StatSeq)(implicit formatterState: FormatterState): FormatResult = {
    val StatSeq(selfReferenceOpt: Option[(Expr, Token)], firstStatOpt: Option[Stat], otherStats: List[(Token, Option[Stat])]) = statSeq
    var formatResult: FormatResult = NoFormatResult

    val firstStatFormatterState = statFormatterState(firstStatOpt)

    for ((selfReference, arrow) ← selfReferenceOpt) {
      formatResult ++= format(selfReference)
      for (stat ← firstStatOpt if hiddenPredecessors(stat.firstToken).containsNewline)
        formatResult = formatResult.before(stat.firstToken, firstStatFormatterState.currentIndentLevelInstruction)
    }

    for (stat ← firstStatOpt)
      formatResult ++= format(stat)(firstStatFormatterState)

    for ((semi, otherStatOption) ← otherStats) {

      val otherStatFormatterState = statFormatterState(otherStatOption)

      if (isInferredNewline(semi))
        formatResult = formatResult.formatNewline(semi, otherStatFormatterState.currentIndentLevelInstruction)

      for (otherStat ← otherStatOption) {
        if (!isInferredNewline(semi)) {
          val firstToken = otherStat.firstToken
          val instruction =
            if (hiddenPredecessors(firstToken).containsNewline)
              otherStatFormatterState.currentIndentLevelInstruction
            else
              CompactEnsuringGap
          formatResult = formatResult.before(firstToken, instruction)
        }
        formatResult ++= format(otherStat)(otherStatFormatterState)
      }

    }

    formatResult
  }

  private def format(stat: Stat)(implicit formatterState: FormatterState): FormatResult =
    stat match {
      case expr: Expr                 ⇒ format(expr)
      case fullDefOrDcl: FullDefOrDcl ⇒ format(fullDefOrDcl)
      case import_ : ImportClause     ⇒ format(import_)
      case packageBlock: PackageBlock ⇒ format(packageBlock)
      case _                          ⇒ NoFormatResult // TODO
    }

  def format(packageBlock: PackageBlock)(implicit formatterState: FormatterState): FormatResult = {
    val PackageBlock(packageToken: Token, name: CallExpr, newlineOpt: Option[Token], lbrace: Token, topStats: StatSeq, rbrace: Token) = packageBlock

    var formatResult: FormatResult = NoFormatResult
    formatResult = newlineOpt match {
      case Some(newline) ⇒ formatResult.formatNewline(newline, CompactEnsuringGap)
      case None          ⇒ formatResult.before(lbrace, CompactEnsuringGap)
    }
    val dummyBlock = BlockExpr(lbrace, Right(topStats), rbrace)
    formatResult ++= format(dummyBlock, indent = formattingPreferences(IndentPackageBlocks))
    formatResult
  }

  def format(fullDefOrDcl: FullDefOrDcl)(implicit formatterState: FormatterState): FormatResult = {
    val FullDefOrDcl(annotations: List[Annotation], modifiers: List[Modifier], defOrDcl: DefOrDcl) = fullDefOrDcl
    var formatResult: FormatResult = NoFormatResult
    val preAnnotationFormattingInstruction =
      if (formatterState.inSingleLineBlock)
        CompactEnsuringGap
      else
        formatterState.currentIndentLevelInstruction
    for ((previousAnnotationOpt, annotation, nextAnnotationOpt) ← Utils.withPreviousAndNext(annotations)) {
      formatResult ++= format(annotation)
      for (previousAnnotation ← previousAnnotationOpt) {
        val instruction = if (annotation.newlineOption.isDefined)
          formatterState.currentIndentLevelInstruction
        else
          CompactEnsuringGap
        formatResult = formatResult.before(annotation.firstToken, instruction)
      }
      if (nextAnnotationOpt.isEmpty) {
        val firstPostAnnotationToken = modifiers match {
          case Nil                ⇒ defOrDcl.firstToken
          case (modifier :: rest) ⇒ modifier.firstToken
        }
        val instruction = if (annotation.newlineOption.isDefined)
          formatterState.currentIndentLevelInstruction
        else
          CompactEnsuringGap
        formatResult = formatResult.before(firstPostAnnotationToken, instruction)
      }
    }
    formatResult ++= format(defOrDcl)
    formatResult
  }

  private def format(defOrDcl: DefOrDcl)(implicit formatterState: FormatterState): FormatResult = defOrDcl match {
    case patDefOrDcl: PatDefOrDcl   ⇒ format(patDefOrDcl)
    case typeDefOrDcl: TypeDefOrDcl ⇒ format(typeDefOrDcl)
    case funDefOrDcl: FunDefOrDcl   ⇒ format(funDefOrDcl)
    case tmplDef: TmplDef           ⇒ format(tmplDef)
    case _                          ⇒ NoFormatResult // TODO
  }

  private def format(patDefOrDcl: PatDefOrDcl)(implicit formatterState: FormatterState): FormatResult = {
    var formatResult: FormatResult = NoFormatResult
    val PatDefOrDcl(valOrVarToken: Token, pattern: Expr, otherPatterns: List[(Token, Expr)], typedOpt: Option[(Token, Type)], equalsClauseOption: Option[(Token, Expr)]) = patDefOrDcl
    formatResult ++= format(pattern)
    for ((comma, otherPattern) ← otherPatterns)
      formatResult ++= format(otherPattern)
    for ((colon, type_) ← typedOpt)
      formatResult ++= format(type_)
    for ((equals, body) ← equalsClauseOption) {
      // TODO: Copy and paste from format(FunDefOrDcl)
      val bodyToken = body.firstToken
      val (formatInstruction, exprFormatterState) =
        if (hiddenPredecessors(bodyToken).containsNewline)
          (formatterState.nextIndentLevelInstruction, formatterState.indent)
        else
          (CompactEnsuringGap, formatterState)
      formatResult = formatResult.before(bodyToken, formatInstruction)
      formatResult ++= format(body)(exprFormatterState)
    }
    formatResult
  }

  private def format(typeDefOrDcl: TypeDefOrDcl)(implicit formatterState: FormatterState): FormatResult = format(typeDefOrDcl.contents)

  def format(funDefOrDcl: FunDefOrDcl)(implicit formatterState: FormatterState): FormatResult = {
    // TODO: Lots
    var formatResult: FormatResult = NoFormatResult
    val FunDefOrDcl(defToken: Token, nameToken: Token, typeParamClauseOpt: Option[TypeParamClause], paramClauses: ParamClauses,
      returnTypeOpt: Option[(Token, Type)], funBodyOpt: Option[FunBody], localDef: Boolean) = funDefOrDcl
    for (typeParamClause ← typeParamClauseOpt)
      formatResult ++= format(typeParamClause.contents)
    formatResult ++= formatParamClauses(paramClauses)
    for ((colon, type_) ← returnTypeOpt)
      formatResult ++= format(type_)
    for (funBody ← funBodyOpt) {
      funBody match {
        case ExprFunBody(equals: Token, body: Expr) ⇒ {
          // TODO: see format(PatDefOrDcl)
          val bodyToken = body.firstToken
          val (formatInstruction, exprFormatterState) =
            if (hiddenPredecessors(bodyToken).containsNewline)
              (formatterState.nextIndentLevelInstruction, formatterState.indent)
            else
              (CompactEnsuringGap, formatterState)
          formatResult = formatResult.before(bodyToken, formatInstruction)
          formatResult ++= format(body)(exprFormatterState)
        }
        case ProcFunBody(newlineOpt: Option[Token], bodyBlock: BlockExpr) ⇒ {
          for (newline ← newlineOpt)
            formatResult = formatResult.formatNewline(newline, CompactEnsuringGap)
          if (newlineOpt.isEmpty)
            formatResult = formatResult.before(bodyBlock.firstToken, CompactEnsuringGap)
          // TODO: else?
          formatResult ++= format(bodyBlock)
        }
      }
    }
    formatResult
  }

  def formatParamClauses(paramClauses: ParamClauses, doubleIndentParams: Boolean = false)(implicit formatterState: FormatterState): FormatResult = {
    val ParamClauses(initialNewlineOpt, paramClausesAndNewlines) = paramClauses
    var formatResult: FormatResult = NoFormatResult
    var currentFormatterState = formatterState
    for ((paramClause, newlineOption) ← paramClausesAndNewlines) { // TODO: Newlines. // maybe already done in some cases by format(tmplDef)?

      val (paramClauseFormatResult, newFormatterState) = formatParamClause(paramClause, doubleIndentParams)(currentFormatterState)
      formatResult ++= paramClauseFormatResult
      currentFormatterState = newFormatterState
    }
    formatResult
  }

  private def formatParamClause(paramClause: ParamClause, doubleIndentParams: Boolean = false)(implicit formatterState: FormatterState): (FormatResult, FormatterState) = {
    val ParamClause(lparen, implicitOption, firstParamOption, otherParams, rparen) = paramClause
    val paramIndent = if (doubleIndentParams) 2 else 1
    val relativeToken = paramClause.tokens(1) // TODO
    var formatResult: FormatResult = NoFormatResult
    var paramFormatterState = formatterState
    val alignParameters = formattingPreferences(AlignParameters) && !formattingPreferences(IndentWithTabs)
    for (firstParam ← firstParamOption) {
      val token = implicitOption getOrElse firstParam.firstToken
      if (hiddenPredecessors(token).containsNewline) {
        formatResult = formatResult.before(token, formatterState.indent(paramIndent).currentIndentLevelInstruction)
        paramFormatterState = if (alignParameters) formatterState.alignWithToken(relativeToken) else formatterState.indent(paramIndent)
      } else if (containsNewline(firstParam) && alignParameters)
        paramFormatterState = formatterState.alignWithToken(relativeToken)
      formatResult ++= format(firstParam)(paramFormatterState)
    }

    for ((comma, param) ← otherParams) {
      val token = param.firstToken
      if (hiddenPredecessors(token).containsNewline) {
        paramFormatterState = if (alignParameters) formatterState.alignWithToken(relativeToken) else formatterState.indent(paramIndent)
        formatResult = formatResult.before(token, paramFormatterState.currentIndentLevelInstruction)
      }
      formatResult ++= format(param)(paramFormatterState)
    }
    (formatResult, paramFormatterState)
  }

  private def format(param: Param)(implicit formatterState: FormatterState): FormatResult = {
    val Param(annotations: List[Annotation], modifiers: List[Modifier], valOrVarOpt: Option[Token], id: Token, paramTypeOpt: Option[(Token, Type)], defaultValueOpt: Option[(Token, Expr)]) = param
    var formatResult: FormatResult = NoFormatResult
    for (annotation ← annotations)
      formatResult ++= format(annotation)
    for ((colon, paramType) ← paramTypeOpt)
      formatResult ++= format(paramType)
    for ((equals, expr) ← defaultValueOpt)
      formatResult ++= format(expr)
    formatResult
  }

  protected def format(import_ : ImportClause)(implicit formatterState: FormatterState): FormatResult = {
    val ImportClause(importToken: Token, importExpr: ImportExpr, otherImportExprs: List[(Token, ImportExpr)]) = import_
    var formatResult: FormatResult = NoFormatResult
    formatResult ++= format(importExpr)
    for ((comma, otherImportExpr) ← otherImportExprs)
      formatResult ++= format(otherImportExpr)
    formatResult
  }

  private def format(importExpr: ImportExpr)(implicit formatterState: FormatterState): FormatResult = importExpr match {
    case expr @ Expr(_)                          ⇒ format(expr)
    case blockImportExpr @ BlockImportExpr(_, _) ⇒ format(blockImportExpr)
  }

  private def format(blockImportExpr: BlockImportExpr)(implicit formatterState: FormatterState): FormatResult = {
    val BlockImportExpr(prefixExpr, importSelectors @ ImportSelectors(lbrace, firstImportSelector: Expr, otherImportSelectors: List[(Token, Expr)], rbrace)) = blockImportExpr
    var formatResult: FormatResult = NoFormatResult
    formatResult ++= format(prefixExpr)

    val singleLineBlock = !containsNewline(importSelectors)
    val newFormatterState = formatterState.copy(inSingleLineBlock = singleLineBlock)

    if (singleLineBlock) {
      formatResult ++= format(firstImportSelector)
      for ((comma, otherImportSelector) ← otherImportSelectors)
        formatResult ++= format(otherImportSelector)
    } else {
      formatResult = formatResult.before(firstImportSelector.firstToken, formatterState.nextIndentLevelInstruction)
      formatResult ++= format(firstImportSelector)
      for ((comma, otherImportSelector) ← otherImportSelectors) {
        formatResult = formatResult.before(otherImportSelector.firstToken, formatterState.nextIndentLevelInstruction)
        formatResult ++= format(otherImportSelector)
      }
      formatResult = formatResult.before(rbrace, formatterState.currentIndentLevelInstruction)
    }
    formatResult
  }

}
