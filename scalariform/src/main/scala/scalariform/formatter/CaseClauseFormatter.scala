package scalariform.formatter

import scalariform.lexer.Token
import scalariform.lexer.Tokens._
import scalariform.parser._
import scalariform.utils.Utils
import scalariform.utils.TextEditProcessor
import scalariform.utils.BooleanLang._
import scalariform.formatter.preferences._
import PartialFunction._
import scala.math.{ max, min }

trait CaseClauseFormatter { self: HasFormattingPreferences with ExprFormatter with HasHiddenTokenInfo with ScalaFormatter ⇒

  def format(caseClausesAstNode: CaseClauses)(implicit formatterState: FormatterState): FormatResult = {
    val clauseGroups: List[Either[ConsecutiveSingleLineCaseClauses, CaseClause]] =
      if (formattingPreferences(AlignSingleLineCaseStatements) && !formattingPreferences(IndentWithTabs))
        groupClauses(caseClausesAstNode)
      else
        caseClausesAstNode.caseClauses.map(Right(_))

    var formatResult: FormatResult = NoFormatResult
    var isFirstCaseClause = true

    // We have to decide whether to indent the hidden tokens before the CASE token (or possibly a preceeding
    // NEWLINE token from a prior case block).
    def handleCaseIndent(caseClause: CaseClause) {
      if (!isFirstCaseClause) {
        previousCaseClauseTrailingNewlineOpt(caseClause, caseClausesAstNode) match {
          case Some(newline) ⇒
            formatResult = formatResult.formatNewline(newline, formatterState.currentIndentLevelInstruction)
          case None ⇒
            if (hiddenPredecessors(caseClause.firstToken).containsNewline)
              formatResult = formatResult.before(caseClause.firstToken, formatterState.currentIndentLevelInstruction)
        }
      }
    }

    def formatSingleCaseClause(caseClause: CaseClause) {
      handleCaseIndent(caseClause)
      formatResult ++= formatCaseClause(caseClause)
      isFirstCaseClause = false
    }

    for (clauseGroup ← clauseGroups)
      clauseGroup match {
        case Left(consecutiveClauses @ ConsecutiveSingleLineCaseClauses(caseClauses, largestCasePatternLength, smallestCasePatternLength)) ⇒
          if (consecutiveClauses.patternLengthRange <= formattingPreferences(AlignSingleLineCaseStatements.MaxArrowIndent)) {
            for (caseClause @ CaseClause(casePattern, statSeq) ← caseClauses) {
              handleCaseIndent(caseClause)
              val arrowInstruction = PlaceAtColumn(formatterState.indentLevel, largestCasePatternLength + 1)
              formatResult ++= formatCaseClause(caseClause, Some(arrowInstruction))
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

  private def groupClauses(caseClausesAstNode: CaseClauses): List[Either[ConsecutiveSingleLineCaseClauses, CaseClause]] = {
    val clausesAreMultiline = containsNewline(caseClausesAstNode) || hiddenPredecessors(caseClausesAstNode.firstToken).containsNewline

    def groupClauses(caseClauses: List[CaseClause], first: Boolean): List[Either[ConsecutiveSingleLineCaseClauses, CaseClause]] =
      caseClauses match {
        case Nil ⇒ Nil
        case (caseClause @ CaseClause(casePattern, statSeq)) :: otherClauses ⇒
          val otherClausesGrouped = groupClauses(otherClauses, first = false)

          val formattedCasePattern = {
            val casePatternSource = getSource(casePattern)
            val casePatternFormatResult = formatCasePattern(casePattern)(FormatterState(indentLevel = 0))
            val offset = casePattern.firstToken.offset
            val edits = writeTokens(casePatternSource, casePattern.tokens, casePatternFormatResult, offset)
            TextEditProcessor.runEdits(casePatternSource, edits)
          }

          val newlineBeforeClause = hiddenPredecessors(caseClause.firstToken).containsNewline ||
            previousCaseClauseEndsWithNewline(caseClause, caseClausesAstNode)

          // To evaluate whether a clause body is multiline, we ignore a trailing newline: 
          val clauseBodyIsMultiline = containsNewline(pruneTrailingNewline(statSeq)) ||
            statSeq.firstTokenOption.exists(hiddenPredecessors(_).containsNewline)

          if (formattedCasePattern.contains('\n') || (first && !clausesAreMultiline) || (!first && !newlineBeforeClause) || clauseBodyIsMultiline)
            Right(caseClause) :: otherClausesGrouped
          else {
            val arrowAdjust = (if (formattingPreferences(RewriteArrowSymbols)) 1 else casePattern.arrow.length) + 1
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

  private case class ConsecutiveSingleLineCaseClauses(clauses: List[CaseClause], largestCasePatternLength: Int, smallestCasePatternLength: Int) {
    def prepend(clause: CaseClause, length: Int) =
      ConsecutiveSingleLineCaseClauses(clause :: clauses, max(length, largestCasePatternLength), min(length, smallestCasePatternLength))

    def patternLengthRange = largestCasePatternLength - smallestCasePatternLength

  }

  private def formatCasePattern(casePattern: CasePattern, arrowInstructionOpt: Option[PlaceAtColumn] = None)(implicit formatterState: FormatterState): FormatResult = {
    val CasePattern(caseToken: Token, pattern: Expr, guardOption: Option[Guard], arrow: Token) = casePattern
    var formatResult: FormatResult = NoFormatResult
    formatResult ++= format(pattern)
    for (guard ← guardOption)
      formatResult ++= format(guard)
    arrowInstructionOpt foreach { instruction ⇒ formatResult = formatResult.before(arrow, instruction) }
    formatResult
  }

  private def formatCaseClause(caseClause: CaseClause, arrowInstructionOpt: Option[PlaceAtColumn] = None)(implicit formatterState: FormatterState): FormatResult = {
    val CaseClause(casePattern: CasePattern, statSeq: StatSeq) = caseClause
    var formatResult: FormatResult = NoFormatResult
    formatResult ++= formatCasePattern(casePattern, arrowInstructionOpt)
    val singleExpr =
      cond(statSeq.firstStatOpt) { case Some(Expr(_)) ⇒ true } &&
        cond(statSeq.otherStats) { case Nil | List((_, None)) ⇒ true }
    val indentBlock =
      statSeq.firstTokenOption.isDefined && newlineBefore(statSeq) ||
        containsNewline(statSeq) && !singleExpr
    if (indentBlock)
      formatResult = formatResult.before(statSeq.firstToken, formatterState.nextIndentLevelInstruction)

    val stateForStatSeq = if (singleExpr && !indentBlock) formatterState else formatterState.indent
    formatResult ++= format(statSeq)(stateForStatSeq)

    formatResult
  }

  /**
   * @return a NEWLINE(S) token at the end of the caseClause, if present, else None
   */
  private def getTrailingNewline(caseClause: CaseClause): Option[Token] =
    for {
      (separator, stat) ← caseClause.statSeq.otherStats.lastOption
      if stat.isEmpty
      if separator.isNewline
    } yield separator

  private def previousCaseClauseTrailingNewlineOpt(caseClause: CaseClause, caseClauses: CaseClauses): Option[Token] =
    Utils.pairWithPrevious(caseClauses.caseClauses).collect {
      case (Some(previousClause), `caseClause`) ⇒ previousClause
    }.headOption.flatMap(getTrailingNewline)

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
