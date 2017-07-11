package scalariform.formatter

import scalariform.formatter.Alignment._
import scalariform.lexer.Chars
import scalariform.lexer.Token
import scalariform.lexer.Tokens._
import scalariform.parser._
import scalariform.utils.Utils
import scalariform.formatter.preferences._
import scala.PartialFunction._

trait ExprFormatter { self: HasFormattingPreferences with AnnotationFormatter with HasHiddenTokenInfo with TypeFormatter with TemplateFormatter with ScalaFormatter with XmlFormatter with CaseClauseFormatter ⇒

  def format(expr: Expr)(implicit formatterState: FormatterState): FormatResult = format(expr.contents)

  private def format(exprElements: List[ExprElement])(implicit formatterState: FormatterState): FormatResult = formatExprElements(exprElements)._1

  private def format(exprElement: ExprElement)(implicit formatterState: FormatterState): FormatResult = exprElement match {
    case ifExpr: IfExpr                           ⇒ format(ifExpr)
    case whileExpr: WhileExpr                     ⇒ format(whileExpr)
    case matchExpr: MatchExpr                     ⇒ format(matchExpr)
    case doExpr: DoExpr                           ⇒ format(doExpr)
    case blockExpr: BlockExpr                     ⇒ format(blockExpr, indent = true)
    case forExpr: ForExpr                         ⇒ format(forExpr)
    case tryExpr: TryExpr                         ⇒ format(tryExpr)
    case template: Template                       ⇒ format(template)
    case statSeq: StatSeq                         ⇒ format(statSeq) // TODO: revisit
    case argumentExprs: ArgumentExprs             ⇒ format(argumentExprs)._1
    case anonymousFunction: AnonymousFunction     ⇒ format(anonymousFunction)
    case GeneralTokens(_)                         ⇒ NoFormatResult
    case PrefixExprElement(_)                     ⇒ NoFormatResult
    case infixExpr: InfixExpr                     ⇒ format(infixExpr)._1
    case postfixExpr: PostfixExpr                 ⇒ format(postfixExpr)
    case annotation: Annotation                   ⇒ format(annotation)
    case typeExprElement: TypeExprElement         ⇒ format(typeExprElement.contents)
    case expr: Expr                               ⇒ format(expr.contents)
    case argument: Argument                       ⇒ format(argument.expr)
    case xmlExpr: XmlExpr                         ⇒ format(xmlExpr)
    case parenExpr: ParenExpr                     ⇒ format(parenExpr)._1
    case new_ : New                               ⇒ format(new_.template)
    case callExpr: CallExpr                       ⇒ format(callExpr)._1
    case equalsExpr: EqualsExpr                   ⇒ format(equalsExpr)
    case ascriptionExpr: AscriptionExpr           ⇒ format(ascriptionExpr)
    case stringInterpolation: StringInterpolation ⇒ formatStringInterpolation(stringInterpolation)
    case _                                        ⇒ NoFormatResult
  }

  private def formatStringInterpolation(stringInterpolation: StringInterpolation)(implicit formatterState: FormatterState): FormatResult = {
    val StringInterpolation(_, stringPartsAndScala, terminalString) = stringInterpolation
    var formatResult: FormatResult = NoFormatResult
    formatResult = formatResult.before(terminalString, Compact)
    for ((stringPart, scala) ← stringPartsAndScala) {
      formatResult = formatResult.before(stringPart, Compact)
      formatResult = formatResult.before(scala.firstToken, Compact)
      formatResult ++= format(scala)(formatterState)
      scala match {
        case Expr(List(BlockExpr(lbrace, caseClausesOrStatSeq, rbrace))) if !containsNewline(scala) ⇒
          formatResult = formatResult.before(rbrace, Compact)
          // First token of caseClausesOrStatSeq (or rbrace):
          formatResult = formatResult.before(scala.tokens.drop(1).head, Compact)
        case _ ⇒
      }
    }
    formatResult
  }

  private def format(ascriptionExpr: AscriptionExpr)(implicit formatterState: FormatterState): FormatResult = {
    val AscriptionExpr(left, _, right) = ascriptionExpr
    var formatResult: FormatResult = NoFormatResult
    var currentFormatterState = formatterState
    val (leftFormatResult, updatedFormatterState) = formatExprElements(left)
    currentFormatterState = updatedFormatterState
    formatResult ++= leftFormatResult
    formatResult ++= format(right)(currentFormatterState)
    formatResult
  }

  private def format(equalsExpr: EqualsExpr)(implicit formatterState: FormatterState): FormatResult = {
    val EqualsExpr(lhs, _, rhs) = equalsExpr
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
    val MatchExpr(left, _, block) = matchExpr
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
          val instructionOption = condOpt((previousElement, element)) {
            case (PrefixExprElement(_), _) ⇒ if (Chars.isOperatorPart(element.firstToken.text(0))) CompactEnsuringGap else Compact
            case (Argument(_), _) ⇒ Compact
            case (_, _: ArgumentExprs) if formattingPreferences(PreserveSpaceBeforeArguments) ⇒ CompactPreservingGap // TODO: Probably not needed now with CallExpr
            case (_, elem) if element.firstTokenOption exists { firstToken ⇒ newlineBefore(firstToken) && !(Set(COMMA, COLON) contains firstToken.tokenType) } ⇒
              val isNestedArgument = elem match {
                case Argument(Expr(CallExpr(None, _, _, moreArgs, _) :: tail)) if moreArgs.isEmpty ⇒ false
                case Argument(Expr(EqualsExpr(_, _, Expr(headExpr :: innerTail)) :: tail)) ⇒ headExpr match {
                  case CallExpr(children, _, _, moreArgs, _) ⇒ moreArgs.nonEmpty || headExpr.tokens.exists(tk => hiddenPredecessors(tk).containsNewline)
                  case _                                     ⇒ headExpr.tokens.exists(tk => hiddenPredecessors(tk).containsNewline)
                }
                case _ ⇒ true
              }

              if (isNestedArgument)
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
            expressionBreakHappened = expressionBreakHappened || newFormatterState.expressionBreakHappened
          )
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
            else if (hiddenPredecessors(token).containsNewline && !(Set(COMMA, COLON) contains token.tokenType)) { // TODO: Probably not needed now, see above
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
    val CallExpr(exprDotOpt, id, typeArgsOpt, newLineOptsAndArgumentExprss, uscoreOpt) = callExpr
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
        expressionBreakHappened = expressionBreakHappened || argUpdatedFormatterState.expressionBreakHappened
      )

      formatResult ++= argResult
    }

    for (uscore ← uscoreOpt)
      formatResult = formatResult.before(uscore, CompactPreservingGap)

    (formatResult, currentFormatterState)
  }

  private def format(infixExpr: InfixExpr)(implicit initialFormatterState: FormatterState): (FormatResult, FormatterState) = {
    val InfixExpr(left, infixId, newlineOption, right) = infixExpr
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
    val PostfixExpr(first, postfixId) = postfixExpr
    var formatResult: FormatResult = NoFormatResult
    formatResult ++= format(first)
    formatResult = formatResult.before(postfixId, CompactPreservingGap)
    formatResult
  }

  def format(anonymousFunction: AnonymousFunction)(implicit formatterState: FormatterState): FormatResult = { // <-- Also formatted specially in BlockExpr
    val AnonymousFunction(parameters, _, body) = anonymousFunction
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
    case args @ ParenArgumentExprs(lparen, contents, _) ⇒
      var currentFormatterState = formatterState
      var formatResult: FormatResult = NoFormatResult

      val (contentsFormatResult, updatedFormatterState) = formatExprElements(GeneralTokens(List(lparen)) :: contents)(currentFormatterState)
      formatResult ++= contentsFormatResult
      currentFormatterState = updatedFormatterState

      val alignedFormatResult = alignArguments(args)

      formatResult ++= alignedFormatResult

      (formatResult, currentFormatterState)
  }

  def calculateEqualsExprIdLength(equalsExpr: EqualsExpr): Int = {
    val maybeLength = condOpt(equalsExpr.lhs) {
      case List(callExpr: CallExpr) ⇒ callExpr.id.length
    }
    maybeLength.getOrElse(0)
  }

  protected def alignArguments(parenArguments: ParenArgumentExprs)(implicit formatterState: FormatterState): FormatResult = {
    val alignArgsEnabled = formattingPreferences(AlignArguments) && !formattingPreferences(IndentWithTabs)
    var formatResult: FormatResult = NoFormatResult

    var argumentFormatterState = formatterState
    val ParenArgumentExprs(_, contents, rparen) = parenArguments

    val firstTwoArguments = contents.iterator.filter { x ⇒
      cond(x) {
        case Argument(_)            ⇒ true
        case callExpr: CallExpr     ⇒ true
        case equalsExpr: EqualsExpr ⇒ true
      }
    }.take(2).toList
    firstTwoArguments match {
      case firstArgument :: secondArgument :: _ =>
        if (hiddenPredecessors(secondArgument.firstToken).containsNewline) {
          formattingPreferences(FirstArgumentOnNewline) match {
            case Force =>
              formatResult ++= formatResult.before(firstArgument.firstToken, argumentFormatterState.nextIndentLevelInstruction)
            case Prevent =>
              formatResult ++= formatResult.before(firstArgument.firstToken, Compact)
            case Preserve => // no-op.
          }
        }
      case _ => // no-op
    }

    var currentExprNewlineCount = 0

    // TODO: refactor this as well as "def groupParams" to share logic
    val eitherAlignableArguments = (contents.foldLeft(List[EitherAlignableEqualsExpr]()) { (existingExprs, exprElement) ⇒
      if (!alignArgsEnabled) {
        // If we're not aligning argument, place each alignable element in its own group.
        val nextExprsOption = condOpt(exprElement) {
          case Argument(Expr(List(callExpr: CallExpr))) ⇒
            Right(callExpr) :: existingExprs
          case Argument(Expr(List(equalsExpr: EqualsExpr))) ⇒
            Left(ConsecutiveSingleLineEqualsExprs(List(equalsExpr), calculateEqualsExprIdLength(equalsExpr))) :: existingExprs
        }
        nextExprsOption.getOrElse(existingExprs)
      } else {
        exprElement match {
          // Equals expressions are clustered into groups.
          case Argument(Expr(List(equalsExpr: EqualsExpr))) ⇒
            currentExprNewlineCount = hiddenPredecessors(exprElement.firstToken).text.count(_ == '\n')
            existingExprs match {
              case _ if currentExprNewlineCount >= 2 =>
                Left(ConsecutiveSingleLineEqualsExprs(List(equalsExpr), calculateEqualsExprIdLength(equalsExpr))) :: existingExprs
              case Left(exprs: ConsecutiveSingleLineEqualsExprs) :: tail ⇒
                Left(exprs.prepend(equalsExpr, calculateEqualsExprIdLength(equalsExpr))) :: tail
              case Right(_) :: _ ⇒
                Left(ConsecutiveSingleLineEqualsExprs(List(equalsExpr), calculateEqualsExprIdLength(equalsExpr))) :: existingExprs
              case Nil ⇒
                Left(ConsecutiveSingleLineEqualsExprs(List(equalsExpr), calculateEqualsExprIdLength(equalsExpr))) :: existingExprs
            }
          // Non-equals expressions translate to a new `Right` group.
          case Argument(Expr(List(callExpr: CallExpr))) ⇒
            currentExprNewlineCount = hiddenPredecessors(exprElement.firstToken).text.count(_ == '\n')
            Right(callExpr) :: existingExprs
          case _ ⇒ existingExprs
        }
      }
    }).reverse map {
      // Reverse the sub-expressions to be in source-order.
      case Left(ConsecutiveSingleLineEqualsExprs(exprs, length)) =>
        Left(ConsecutiveSingleLineEqualsExprs(exprs.reverse, length))
      case right @ Right(_) => right
    }

    // Separate out the first argument for special processing.
    val (firstArgumentOption, maxIdLengthOption, otherAlignableArguments) = {
      eitherAlignableArguments.headOption match {
        case Some(Left(consecutiveArgs)) =>
          val (firstArgumentOption, remainingArguments) = consecutiveArgs match {
            case ConsecutiveSingleLineEqualsExprs(firstArg :: remaining, _) =>
              (Some(firstArg), consecutiveArgs.copy(equalsExprs = remaining))
            case ConsecutiveSingleLineEqualsExprs(Nil, _) => (None, consecutiveArgs)
          }
          val otherAlignableArguments = Left(remainingArguments) :: eitherAlignableArguments.tail
          (firstArgumentOption, Some(remainingArguments.largestIdLength), otherAlignableArguments)
        case Some(Right(singleArg)) =>
          (Some(singleArg), None, eitherAlignableArguments.tail)
        case None => (None, None, Nil)
      }
    }

    firstArgumentOption foreach { firstArgument =>
      val firstToken = firstArgument.firstToken
      if ((hiddenPredecessors(firstToken).containsNewline || formatResult.tokenWillHaveNewline(firstToken)) &&
        formattingPreferences(FirstArgumentOnNewline) != Prevent) {
        // Indent appropriately.
        formatResult ++= formatResult.before(firstToken, argumentFormatterState.nextIndentLevelInstruction)
      }

      // Additional tokens should align with this argument's placing.
      if (alignArgsEnabled)
        argumentFormatterState = argumentFormatterState.alignWithToken(firstToken)

      firstArgument match {
        case equalsExpr: EqualsExpr =>
          // Indent the equals sign to align with the rightmost equals.
          if (!formattingPreferences(IndentWithTabs) && maxIdLengthOption.nonEmpty)
            formatResult ++= formatResult.before(
              equalsExpr.equals,
              PlaceAtColumn(
                0,
                maxIdLengthOption.get + 1,
                Some(firstToken)
              )
            )

        case _ => // no-op
      }
    }

    otherAlignableArguments foreach {
      case Left(ConsecutiveSingleLineEqualsExprs(exprs, maxIdLength)) ⇒
        exprs foreach { expr ⇒
          val firstToken = expr.firstToken

          // If there's a newline before this argument, OR there will be a newline after formatting
          if (hiddenPredecessors(firstToken).containsNewline || formatResult.tokenWillHaveNewline(firstToken)) {
            if (alignArgsEnabled) {
              // Align with the first token (set above).
              formatResult ++= formatResult.before(firstToken, argumentFormatterState.currentIndentLevelInstruction)
            } else {
              // Start a newline, indenting to the correct level.
              formatResult ++= formatResult.before(firstToken, argumentFormatterState.nextIndentLevelInstruction)
            }
            // Indent the equals sign to align with the rightmost equals.
            if (!formattingPreferences(IndentWithTabs))
              formatResult ++= formatResult.before(
                expr.equals,
                PlaceAtColumn(
                  0,
                  maxIdLength + 1,
                  Some(firstToken)
                )
              )
          }
        }
      case Right(callExpr) ⇒
        val firstToken = callExpr.firstToken
        if (hiddenPredecessors(firstToken).containsNewline) {
          if (alignArgsEnabled) {
            // Align with the first token (set above).
            formatResult ++= formatResult.before(firstToken, argumentFormatterState.currentIndentLevelInstruction)
          } else {
            // Start a newline, indenting to the correct level.
            formatResult ++= formatResult.before(firstToken, argumentFormatterState.nextIndentLevelInstruction)
          }
        }
    }

    // Handle the closing lparen. We check if the first token (of any time) is on a newline, or if
    // the second argument-like token is on a newline.
    val firstTokensAreOnNewline = (contents.headOption ++ firstTwoArguments.lastOption).exists { x ⇒
      val firstToken = x.firstToken
      val forceNewline = formattingPreferences(DanglingCloseParenthesis) == Force
      hiddenPredecessors(rparen).containsComment ||
        forceNewline && (hiddenPredecessors(firstToken).containsNewline || formatResult.tokenWillHaveNewline(firstToken))
    }
    val shouldPreserveNewline =
      (formattingPreferences(DanglingCloseParenthesis) == Preserve) &&
        hiddenPredecessors(rparen).containsNewline &&
        contents.nonEmpty
    if (firstTokensAreOnNewline || shouldPreserveNewline)
      formatResult ++= formatResult.before(rparen, formatterState.currentIndentLevelInstruction)

    formatResult
  }

  private def format(parenExpr: ParenExpr)(implicit formatterState: FormatterState): (FormatResult, FormatterState) = {
    val ParenExpr(lparen, contents, rparen) = parenExpr
    format(ParenArgumentExprs(lparen, contents, rparen))
  }

  private def format(tryExpr: TryExpr)(implicit formatterState: FormatterState): FormatResult = {
    val TryExpr(_, body, catchClauseOption, finallyClauseOption) = tryExpr
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
    val IfExpr(_, condExpr, newlinesOpt, body, elseClauseOption) = ifExpr
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
    val CondExpr(_, condition, _) = condExpr
    format(condition)
  }

  private def isIfExpr(expr: Expr) = expr.contents.size == 1 && expr.contents(0).isInstanceOf[IfExpr]

  private def format(forExpr: ForExpr)(implicit formatterState: FormatterState): FormatResult = {
    val ForExpr(_, _, enumerators, rParenOrBrace, newlinesOpt, yieldOption, body) = forExpr
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
    val Enumerators(initialGenerator, rest) = enumerators
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
    val Generator(_, pattern, _, expr, guards) = generator
    var formatResult: FormatResult = NoFormatResult
    formatResult ++= format(expr)
    formatResult ++= format(pattern)
    for (guard ← guards)
      formatResult ++= format(guard)
    formatResult
  }

  def format(guard: Guard)(implicit formatterState: FormatterState): FormatResult = {
    val Guard(_, expr) = guard
    format(expr)
  }

  private def format(whileExpr: WhileExpr)(implicit formatterState: FormatterState): FormatResult = {
    // TODO: Same as first half of ifExpr
    val WhileExpr(_, condExpr, newlinesOpt, body) = whileExpr
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
    val DoExpr(_, body, statSepOpt, whileToken, condExpr) = doExpr

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
    val BlockExpr(_, caseClausesOrStatSeq, rbrace) = blockExpr
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
                def hasNestedAnonymousFunction(subStatSeq: StatSeq): Boolean =
                  (for {
                    firstStat <- subStatSeq.firstStatOpt
                    head <- firstStat.immediateChildren.headOption
                  } yield head.isInstanceOf[AnonymousFunction]).getOrElse(false)

                val (instruction, subStatState) =
                  if (hasNestedAnonymousFunction(subStatSeq))
                    (CompactEnsuringGap, indentedState.indent(-1))
                  else if (hiddenPredecessors(params(0).firstToken).containsNewline)
                    (indentedInstruction, indentedState.indent)
                  else
                    (CompactEnsuringGap, indentedState)
                formatResult = formatResult.before(statSeq.firstToken, instruction)
                formatResult ++= format(params)
                for (firstToken ← subStatSeq.firstTokenOption) {
                  val instruction =
                    if (hasNestedAnonymousFunction(subStatSeq))
                      CompactEnsuringGap
                    else if (hiddenPredecessors(firstToken).containsNewline || containsNewline(subStatSeq))
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
    val StatSeq(selfReferenceOpt, firstStatOpt, otherStats) = statSeq
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
    val PackageBlock(_, _, newlineOpt, lbrace, topStats, rbrace) = packageBlock

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
    val FullDefOrDcl(annotations, modifiers, defOrDcl) = fullDefOrDcl
    var formatResult: FormatResult = NoFormatResult
    for ((previousAnnotationOpt, annotation, nextAnnotationOpt) ← Utils.withPreviousAndNext(annotations)) {
      formatResult ++= format(annotation)
      for (previousAnnotation ← previousAnnotationOpt) {
        val instruction = if (previousAnnotation.newlineOption.isDefined)
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
    // Ensure spacing after any modifiers. If the last modifier is simply a newline, skip the gap,
    // as this will already cause a space to be added.
    // TODO: Update the `Modifier` parsing to handle newlines more gracefully, instead of putting
    // them in a dummy `SimpleModifier`.
    if (modifiers.nonEmpty && !modifiers.last.firstToken.tokenType.isNewline) {
      formatResult = formatResult.before(defOrDcl.firstToken, CompactEnsuringGap)
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
    val PatDefOrDcl(_, pattern, otherPatterns, typedOpt, equalsClauseOption) = patDefOrDcl
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
    val FunDefOrDcl(_, _, typeParamClauseOpt, paramClauses, returnTypeOpt, funBodyOpt, _) = funDefOrDcl
    for (typeParamClause ← typeParamClauseOpt)
      formatResult ++= format(typeParamClause.contents)
    val doubleIndentParams = formattingPreferences(DoubleIndentMethodDeclaration)
    formatResult ++= formatParamClauses(paramClauses, doubleIndentParams)
    for ((colon, type_) ← returnTypeOpt)
      formatResult ++= format(type_)
    for (funBody ← funBodyOpt) {
      funBody match {
        case ExprFunBody(equals, macroOpt, body) ⇒ {
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
        case ProcFunBody(newlineOpt, bodyBlock) ⇒ {
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
    val ParamClauses(_, paramClausesAndNewlines) = paramClauses
    var formatResult: FormatResult = NoFormatResult
    val currentFormatterState = formatterState
    for ((paramClause, newlineOption) ← paramClausesAndNewlines) { // TODO: Newlines. // maybe already done in some cases by format(tmplDef)?
      val (paramClauseFormatResult, _) = formatParamClause(paramClause, doubleIndentParams)(currentFormatterState)
      formatResult ++= paramClauseFormatResult
    }
    formatResult
  }

  // TODO: Parts of this function might be useful in implementing other alignment features
  protected def calculateParamSectionLengths(param: Param, first: Boolean)(implicit formatterState: FormatterState): Option[ParamSectionLengths] = {
    val Param(annotations, modifiers, valOrVarOpt, id, paramTypeOpt, _) = param

    val formattedParam = formattedAstNode(param) {
      format(param)(formatterState)
    }

    def calculateLengths: ParamSectionLengths = {
      def calculatePrefixLength: Int = {
        // Calculate longest "prefix" length. Annotations, modifiers, and val/var contribute
        // to this number.
        val allPrefixTokens =
          annotations.flatMap(_.tokens) ++
            modifiers.flatMap(_.tokens) ++
            valOrVarOpt
        var prefixLength = 0
        allPrefixTokens.filter(!_.isNewline).foreach {
          prefixLength += _.length
        }

        // Account for whitespace between prefix token types. This assumes everything
        // will be placed on a single line with no newlines between prefixes.
        val numberOfPrefixTypes = Seq(
          !annotations.isEmpty,
          valOrVarOpt.isDefined
        ).count(_ == true) + modifiers.length
        if (numberOfPrefixTypes > 0)
          prefixLength += numberOfPrefixTypes - 1

        prefixLength
      }

      // Account for the colon (and possible space) after the parameter's name
      def calculateIdLength: Int = {
        if (formattingPreferences(SpaceBeforeColon))
          id.length + 2
        else
          id.length + 1
      }

      def calculateTypeLength: Int = {
        // Calculate longest "type" length.
        val typeLengthOpt = paramTypeOpt map {
          case (_, typeAst) ⇒
            val formattedType = formattedAstNode(typeAst) {
              format(typeAst)(formatterState)
            }
            formattedType.length
        }
        typeLengthOpt.getOrElse(0)
      }

      val prefixLength = calculatePrefixLength
      val idLength = calculateIdLength
      val prefixAndIdLength = {
        if (prefixLength > 0)
          prefixLength + idLength + 1 // the + 1 is to account for whitespace
        else
          idLength
      }

      ParamSectionLengths(prefixLength, idLength, prefixAndIdLength, calculateTypeLength)
    }

    val newlineBeforeParam = hiddenPredecessors(param.firstToken).containsNewline

    if (formattedParam.contains('\n') || (!first && !newlineBeforeParam))
      None
    else
      Some(calculateLengths)
  }

  /**
   * Groups consecutive single line params in a [[scalariform.parser.ParamClause]] for alignment.
   * The head of the return value (and head of the params list in the returned ConsecutiveSingleLineParams is guaranteed
   * to be the very first parameter in the paramClause. The other parameters are not necessarily in the order they appear.
   * @param paramClause
   * @param formatterState
   *
   * @return List of grouped params. Left stores a group of parameters that can be aligned together,
   *         Right stores an unalignable param.
   */
  private def groupParams(paramClause: ParamClause, alignParameters: Boolean)(implicit formatterState: FormatterState): List[EitherAlignableParam] = {
    val ParamClause(_, implicitOption, firstParamOption, otherParamsWithComma, _) = paramClause

    val otherParams = otherParamsWithComma.map { case (comma, param) ⇒ param }

    // This is reversed because "appendParamToGroup" works on lists, and will
    // create the list in the reverse order of the list it is given.
    val allParams = (firstParamOption.toList ++ otherParams).reverse

    def appendParamToGroup(
      previousParam: Option[Param],
      paramToAppend: Param,
      nextParam:     Option[Param],
      groupedParams: List[EitherAlignableParam]
    ): List[EitherAlignableParam] = {

      // This unintuitive line is dependent on the ordering of groupedParams being passed
      // in. It's in reverse.
      val isFirstParam = !nextParam.isDefined

      val firstParamAlignable = !implicitOption.isDefined ||
        (newlineBefore(implicitOption.get) && otherParams != Nil && newlineBefore(otherParams.head))

      val paramIsAlignable = alignParameters && (!isFirstParam || firstParamAlignable)

      if (alignParameters && paramIsAlignable) {
        calculateParamSectionLengths(paramToAppend, isFirstParam) match {
          case Some(sectionLengths) ⇒
            groupedParams match {
              case Right(param) :: tail ⇒
                Left(ConsecutiveSingleLineParams(List(paramToAppend), sectionLengths, sectionLengths)) :: groupedParams
              case Left(existingParams) :: tail ⇒
                if (previousParam.isDefined) {
                  /* Group params separately if a blank line between two params:
                   * case class Spacing(a:   Int = 1,
                   *                    bee: Int = 2,
                   *
                   *                    ceee:  String = "",
                   *                    deeee: Any    = Nothing)
                   */
                  val numNewlinesBeforeParam = hiddenPredecessors(previousParam.get.firstToken).text.count(_ == '\n')
                  if (numNewlinesBeforeParam >= 2)
                    Left(ConsecutiveSingleLineParams(List(paramToAppend), sectionLengths, sectionLengths)) :: groupedParams
                  else
                    Left(existingParams.prepend(paramToAppend, sectionLengths)) :: tail
                } else {
                  Left(existingParams.prepend(paramToAppend, sectionLengths)) :: tail
                }
              case Nil ⇒
                Left(ConsecutiveSingleLineParams(List(paramToAppend), sectionLengths, sectionLengths)) :: Nil
            }
          case None ⇒
            Right(paramToAppend) :: groupedParams
        }
      } else {
        Right(paramToAppend) :: groupedParams
      }
    }

    val staggeredParams = Utils.withPreviousAndNext(allParams)

    val paramsGroup = staggeredParams.foldLeft(List[EitherAlignableParam]()) { (groupedParams, prevAndNext) ⇒
      val (prevParam, param, nextParam) = prevAndNext
      appendParamToGroup(prevParam, param, nextParam, groupedParams)
    }

    paramsGroup
  }

  private def formatParamClause(paramClause: ParamClause, doubleIndentParams: Boolean)(implicit formatterState: FormatterState): (FormatResult, FormatterState) = {
    val ParamClause(_, implicitOption, firstParamOption, otherParams, rparen) = paramClause
    val paramIndent = if (doubleIndentParams) 2 else 1
    val relativeToken = paramClause.tokens(1) // TODO
    var formatResult: FormatResult = NoFormatResult
    var paramFormatterState = formatterState
    val alignParameters = formattingPreferences(AlignParameters) && !formattingPreferences(IndentWithTabs)

    // True if the arguments span multiple lines.
    // TODO: This is actually a broken boolean; it only gets set to true if the second argument is
    // on a newline, but it should check *all* arguments.
    val multilineArguments = {
      val secondParamOption = otherParams.headOption map { case (_, param) => param }
      (firstParamOption, secondParamOption) match {
        case (Some(_), Some(secondParam)) => hiddenPredecessors(secondParam.firstToken).containsNewline
        case _                            => false
      }
    }

    // Handle placing the first argument on a newline, if requested.
    if (multilineArguments) {
      formattingPreferences(FirstParameterOnNewline) match {
        case Force =>
          formatResult ++= formatResult.before(
            firstParamOption.get.firstToken,
            paramFormatterState.indent(paramIndent).currentIndentLevelInstruction
          )
        case Prevent =>
          formatResult ++= formatResult.before(firstParamOption.get.firstToken, Compact)
        case Preserve => // no-op.
      }
    }

    val hasContent = implicitOption.isDefined || firstParamOption.isDefined

    val shouldIndentParen = hiddenPredecessors(rparen).containsComment ||
      ((hiddenPredecessors(rparen).containsNewline &&
        formattingPreferences(DanglingCloseParenthesis) == Preserve) ||
        formattingPreferences(DanglingCloseParenthesis) == Force)

    val firstTokenIsOnNewline = hiddenPredecessors(relativeToken).containsNewline ||
      formatResult.tokenWillHaveNewline(relativeToken)

    // Place rparen on its own line if this is a multi-line param clause with content, and the
    // preferences say to.
    if ((multilineArguments || firstTokenIsOnNewline) && hasContent && shouldIndentParen)
      formatResult = formatResult.before(rparen, paramFormatterState.currentIndentLevelInstruction)

    val groupedParams = groupParams(paramClause, alignParameters)

    // Separate the first parameter from the groupedParams, since we have to
    // do special formatting for the first param.
    val (firstGroupedParamOption, maxSectionLengthsOption, otherGroupedParams) = groupedParams.headOption match {
      case Some(Left(consecutiveParams)) ⇒
        // First parameter is part of a group of params; split it off of the group.
        val (firstParam, remainingConsecutiveParams) = consecutiveParams.pop
        val maxSectionLengths = remainingConsecutiveParams.maxSectionLengths
        val otherGroupedParams = Left(remainingConsecutiveParams) :: groupedParams.tail
        (firstParam, Some(maxSectionLengths), otherGroupedParams)
      case Some(Right(param)) ⇒ (Some(param), None, groupedParams.tail)
      case None               ⇒ (None, None, Nil)
    }

    for (firstParam ← firstGroupedParamOption) {
      maxSectionLengthsOption match {
        case Some(lengths) ⇒

          // Indent prefixes (annotations, modifiers, and id) if alignParameters is enabled
          alignFirstParam(firstParam)

          // Indent Type
          indentType(firstParam, lengths)

          // Indent Default
          indentDefault(firstParam, lengths)
        case None ⇒
          alignFirstParam(firstParam)
      }
      formatResult ++= format(firstParam)(paramFormatterState)
    }

    otherGroupedParams.foreach {
      case Left(ConsecutiveSingleLineParams(params, maxSectionLengths, thisSectionLengths)) ⇒
        params.foreach { param ⇒
          val firstToken = param.firstToken

          // Indent prefixes (annotations, modifiers, and id)
          alignOtherParams(firstToken)

          // Indent Type
          indentType(param, maxSectionLengths)

          // Indent Default
          indentDefault(param, maxSectionLengths)

          formatResult ++= format(param)(paramFormatterState)
        }
      case Right(param) ⇒
        alignOtherParams(param.firstToken)
        formatResult ++= format(param)(paramFormatterState)
    }

    def alignFirstParam(firstParam: Param) = {
      // Place implicit on its own line.
      for (implicitToken ← implicitOption) {
        if (hiddenPredecessors(implicitToken).containsNewline || (containsNewline(firstParam) && alignParameters))
          formatResult = formatResult.before(implicitToken, paramFormatterState.indent(paramIndent).currentIndentLevelInstruction)
      }

      val firstToken = firstParam.firstToken
      val implicitOrFirstToken = implicitOption getOrElse firstToken

      if (alignParameters)
        paramFormatterState = formatterState.alignWithToken(relativeToken)

      if (hiddenPredecessors(implicitOrFirstToken).containsNewline) {
        if (formattingPreferences(FirstParameterOnNewline) != Prevent)
          formatResult = formatResult.before(firstToken, formatterState.indent(paramIndent).currentIndentLevelInstruction)
        if (!alignParameters)
          paramFormatterState = formatterState.indent(paramIndent)
      } else if (containsNewline(firstParam) && alignParameters) {
        paramFormatterState = formatterState.alignWithToken(relativeToken)
      }
    }

    def alignOtherParams(firstToken: Token) = {
      if (hiddenPredecessors(firstToken).containsNewline) {
        paramFormatterState = if (alignParameters) formatterState.alignWithToken(relativeToken) else formatterState.indent(paramIndent)
        formatResult = formatResult.before(firstToken, paramFormatterState.currentIndentLevelInstruction)
      }
    }

    def indentType(param: Param, maxSectionLengths: ParamSectionLengths) = {
      for ((colon, typeAst) ← param.paramTypeOpt) {
        val typeSpaces = maxSectionLengths.prefixAndIdLength + 1
        formatResult = formatResult.before(
          typeAst.firstToken,
          PlaceAtColumn(
            0,
            typeSpaces,
            paramFormatterState.indentRelativeToTokenOption
          )
        )
      }
    }

    def indentDefault(param: Param, maxSectionLengths: ParamSectionLengths) = {
      for ((equal, default) ← param.defaultValueOpt) {
        val defaultSpaces = {
          maxSectionLengths.prefixAndIdLength +
            maxSectionLengths.typeLength + 2
        }
        formatResult = formatResult.before(
          equal,
          PlaceAtColumn(
            0,
            defaultSpaces,
            paramFormatterState.indentRelativeToTokenOption
          )
        )
      }
    }

    (formatResult, paramFormatterState)

  }

  private def format(param: Param)(implicit formatterState: FormatterState): FormatResult = {
    val Param(annotations, _, _, _, paramTypeOpt, defaultValueOpt) = param
    var formatResult: FormatResult = NoFormatResult

    for (annotation ← annotations) {
      formatResult ++= format(annotation)
    }
    for ((colon, paramType) ← paramTypeOpt) {
      formatResult ++= format(paramType)
    }
    for ((equals, expr) ← defaultValueOpt) {
      formatResult ++= format(expr)
    }
    formatResult
  }

  protected def format(import_ :ImportClause)(implicit formatterState: FormatterState): FormatResult = {
    val ImportClause(_, importExpr, otherImportExprs) = import_
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
    val BlockImportExpr(prefixExpr, importSelectors @ ImportSelectors(_, firstImportSelector, otherImportSelectors, rbrace)) = blockImportExpr
    var formatResult: FormatResult = NoFormatResult

    formatResult ++= format(prefixExpr)

    val singleLineBlock = !containsNewline(importSelectors)

    if (singleLineBlock) {
      // We are in a braced import statement like "import foo.{bar=>baz}"
      // or "import foo.{a,b,c}".
      // The default formatting instruction for a LBRACE is "CompactEnsuringGap"
      // (See ScalaFormatter.actualDefaultFormattingInstruction), so if we don't
      // emit an overriding instruction here, then a space will be inserted.
      // That's against the Scala Style Guide, so we need to use the extra
      // context we have at this time to mark this as not a normal LBRACE.
      if (!formattingPreferences(SpacesAroundMultiImports))
        formatResult = formatResult.before(firstImportSelector.firstToken, Compact)

      formatResult ++= format(firstImportSelector)
      for ((comma, otherImportSelector) ← otherImportSelectors)
        formatResult ++= format(otherImportSelector)

      if (!formattingPreferences(SpacesAroundMultiImports))
        formatResult = formatResult.before(rbrace, Compact)
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
