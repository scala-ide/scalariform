package scalariform.formatter

import scala.PartialFunction._
import scalariform.formatter.Alignment._
import scalariform.formatter.preferences._
import scalariform.lexer.Token
import scalariform.parser._
import scalariform.utils.Utils

trait CaseClauseFormatter { self: HasFormattingPreferences with ExprFormatter with HasHiddenTokenInfo with ScalaFormatter ⇒

  def format(caseClausesAstNode: CaseClauses)(implicit formatterState: FormatterState): FormatResult = {
    val clauseGroups: List[Either[ConsecutiveSingleLineCaseClauses, CaseClause]] =
      if (formattingPreferences(AlignSingleLineCaseStatements) && !formattingPreferences(IndentWithTabs))
        groupClauses(caseClausesAstNode)
      else
        caseClausesAstNode.caseClauses.map(Right(_))

    var formatResult: FormatResult = NoFormatResult
    var isFirstCaseClause = true
    val hasSingleCaseClause = clauseGroups.size == 1

    // We have to decide whether to indent the hidden tokens before the CASE token (or possibly a preceding
    // NEWLINE token from a prior case block).
    def handleCaseIndent(caseClause: CaseClause): Unit =
      if (!isFirstCaseClause) {
        previousCaseClauseTrailingNewlineOpt(caseClause, caseClausesAstNode) match {
          case Some(newline) ⇒
            formatResult = formatResult.formatNewline(newline, formatterState.currentIndentLevelInstruction)
          case None ⇒
            if (hiddenPredecessors(caseClause.firstToken).containsNewline)
              formatResult = formatResult.before(caseClause.firstToken, formatterState.currentIndentLevelInstruction)
        }
      }

    def formatSingleCaseClause(caseClause: CaseClause): Unit = {
      handleCaseIndent(caseClause)
      formatResult ++= formatCaseClause(caseClause, None, hasSingleCaseClause)
      isFirstCaseClause = false
    }

    for (clauseGroup ← clauseGroups)
      clauseGroup match {
        case Left(consecutiveClauses @ ConsecutiveSingleLineCaseClauses(caseClauses, largestCasePatternLength, _)) ⇒
          if (consecutiveClauses.patternLengthRange <= formattingPreferences(AlignSingleLineCaseStatements.MaxArrowIndent)) {
            for (caseClause @ CaseClause(_, _) ← caseClauses) {
              handleCaseIndent(caseClause)
              val arrowInstruction = PlaceAtColumn(formatterState.indentLevel, largestCasePatternLength + 1)
              formatResult ++= formatCaseClause(
                caseClause, Some(arrowInstruction), hasSingleCaseClause
              )
              isFirstCaseClause = false
            }
          } else {
            caseClauses foreach formatSingleCaseClause
          }
        case Right(caseClause) ⇒
          formatSingleCaseClause(caseClause)
      }
    formatResult
  }

  private def groupClauses(caseClausesAstNode: CaseClauses): List[EitherAlignableCaseClause] = {
    val clausesAreMultiline = containsNewline(caseClausesAstNode) || hiddenPredecessors(caseClausesAstNode.firstToken).containsNewline

    def groupClauses(caseClauses: List[CaseClause], first: Boolean): List[EitherAlignableCaseClause] =
      caseClauses match {
        case Nil ⇒ Nil
        case (caseClause @ CaseClause(casePattern, statSeq)) :: otherClauses ⇒
          val otherClausesGrouped = groupClauses(otherClauses, first = false)

          val formattedCasePattern = formattedAstNode(casePattern) {
            formatCasePattern(casePattern)(FormatterState(indentLevel = 0))
          }

          val newlineBeforeClause = hiddenPredecessors(caseClause.firstToken).containsNewline ||
            previousCaseClauseEndsWithNewline(caseClause, caseClausesAstNode)

          // To evaluate whether a clause body is multiline, we ignore a trailing newline:
          val clauseBodyIsMultiline = containsNewline(pruneTrailingNewline(statSeq)) ||
            statSeq.firstTokenOption.exists(hiddenPredecessors(_).containsNewline)

          if (formattedCasePattern.contains('\n') || (first && !clausesAreMultiline) || (!first && !newlineBeforeClause) || clauseBodyIsMultiline)
            Right(caseClause) :: otherClausesGrouped
          else {
            val arrowAdjust = 1 + {
              if (formattingPreferences(RewriteArrowSymbols))
                if (formattingPreferences(UseUnicodeArrows)) 1
                else 2
              else casePattern.arrow.length
            }
            val casePatternLength = formattedCasePattern.length - arrowAdjust
            otherClausesGrouped match {
              case Left(consecutiveSingleLineCaseClauses) :: otherGroups ⇒
                Left(consecutiveSingleLineCaseClauses.prepend(caseClause, casePatternLength)) :: otherGroups
              case _ ⇒
                Left(ConsecutiveSingleLineCaseClauses(caseClause :: Nil, casePatternLength, casePatternLength)) :: otherClausesGrouped
            }
          }
      }
    groupClauses(caseClausesAstNode.caseClauses, first = true)
  }

  private def formatCasePattern(casePattern: CasePattern, arrowInstructionOpt: Option[PlaceAtColumn] = None)(implicit formatterState: FormatterState): FormatResult = {
    val CasePattern(_, pattern, guardOption, arrow) = casePattern
    var formatResult: FormatResult = NoFormatResult
    formatResult ++= format(pattern)
    for (guard ← guardOption)
      formatResult ++= format(guard)
    arrowInstructionOpt foreach { instruction ⇒ formatResult = formatResult.before(arrow, instruction) }
    formatResult
  }

  private def formatCaseClause(
    caseClause:          CaseClause,
    arrowInstructionOpt: Option[PlaceAtColumn],
    hasSingleCaseClause: Boolean
  )(implicit formatterState: FormatterState): FormatResult = {

    val CaseClause(casePattern, statSeq) = caseClause
    var formatResult: FormatResult = NoFormatResult
    formatResult ++= formatCasePattern(casePattern, arrowInstructionOpt)
    val hasNewline = caseClause.casePattern.caseToken.associatedWhitespaceAndComments.containsNewline
    val singleCaseWithoutNewline =
      hasSingleCaseClause && !hasNewline && !formattingPreferences(SingleCasePatternOnNewline)
    val singleExpr =
      cond(statSeq.firstStatOpt) { case Some(Expr(_)) ⇒ true } &&
        cond(statSeq.otherStats) { case Nil | List((_, None)) ⇒ true }
    val indentBlock =
      statSeq.firstTokenOption.isDefined && newlineBefore(statSeq) ||
        containsNewline(statSeq) && !singleExpr

    def unindent(x: Map[Token, IntertokenFormatInstruction]): Map[Token, IntertokenFormatInstruction] = x.map {
      case (k, EnsureNewlineAndIndent(indentLevel, relativeTo)) =>
        k -> EnsureNewlineAndIndent(indentLevel - 1, relativeTo)
      case z => z
    }

    if (indentBlock) {
      val result = formatResult.before(statSeq.firstToken, formatterState.nextIndentLevelInstruction)
      formatResult =
        if (!singleCaseWithoutNewline) result
        else
          result.copy(
            predecessorFormatting =
              unindent(result.predecessorFormatting) + ( // unindent first token in case body
                caseClause.casePattern.caseToken -> CompactEnsuringGap // remove `case` leading newline
              )
          )
    }

    val stateForStatSeq = if (singleExpr && !indentBlock) formatterState else formatterState.indent
    formatResult ++= {
      val result = format(statSeq)(stateForStatSeq)
      if (!singleCaseWithoutNewline) result
      else
        result.copy( // unindent body tokens
          predecessorFormatting = unindent(result.predecessorFormatting),
          inferredNewlineFormatting = unindent(result.inferredNewlineFormatting)
        )
    }
    formatResult
  }

  /**
   * @return a NEWLINE(S) token at the end of the caseClause, if present, else None
   */
  private def getTrailingNewline(caseClause: CaseClause): Option[Token] =
    for {
      (separator, stat) ← lastStat(caseClause.statSeq)
      if stat.isEmpty
      if separator.isNewline
    } yield separator

  /**
   * @return the last stat of a block which may be wrapped inside of an anonymous function definition
   */
  def lastStat(statSeq: StatSeq): Option[(Token, Option[Stat])] =
    statSeq.otherStats match {
      case Nil ⇒
        None
        statSeq.firstStatOpt flatMap {
          case Expr(List(AnonymousFunction(_, _, body))) ⇒ lastStat(body)
          case _                                         ⇒ None
        }
      case others ⇒
        others.lastOption match {
          case Some((_, Some(Expr(List(AnonymousFunction(_, _, body)))))) ⇒ lastStat(body)
          case x ⇒ x
        }
    }

  private def previousCaseClauseTrailingNewlineOpt(caseClause: CaseClause, caseClauses: CaseClauses): Option[Token] =
    Utils.pairWithPrevious(caseClauses.caseClauses).collectFirst {
      case (Some(previousClause), `caseClause`) ⇒ previousClause
    }.flatMap(getTrailingNewline)

  private def previousCaseClauseEndsWithNewline(caseClause: CaseClause, caseClauses: CaseClauses): Boolean =
    previousCaseClauseTrailingNewlineOpt(caseClause, caseClauses).isDefined

  /**
   * Remove a trailing NEWLINE / NEWLINES token from the end of the StatSeq.
   */
  private def pruneTrailingNewline(statSeq: StatSeq): StatSeq = statSeq.otherStats.lastOption match {
    case Some((separator, None)) if separator.isNewline ⇒ statSeq.copy(otherStats = statSeq.otherStats.init)
    case _ ⇒ statSeq
  }
}
