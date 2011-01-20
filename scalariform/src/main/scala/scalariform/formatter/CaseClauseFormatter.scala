package scalariform.formatter

import scalariform.lexer.Token
import scalariform.lexer.Tokens._
import scalariform.parser._
import scalariform.utils.Utils
import scalariform.utils.TextEditProcessor
import scalariform.utils.BooleanLang._
import scalariform.formatter.preferences._
import PartialFunction._

trait CaseClauseFormatter { self: HasFormattingPreferences with ExprFormatter with HasHiddenTokenInfo with ScalaFormatter ⇒

  def format(caseClauses: CaseClauses)(implicit formatterState: FormatterState): FormatResult = {
    val clauseGroups = if (formattingPreferences(AlignSingleLineCaseStatements))
      groupClauses(caseClauses)
    else
      caseClauses.caseClauses.map(Right(_))
    var formatResult: FormatResult = NoFormatResult
    var first = true
    for (clauseGroup ← clauseGroups) {
      clauseGroup match {
        case Left(ConsecutiveSingleLineCaseClauses(caseClauses, largestCasePatternLength)) ⇒
          for (caseClause@CaseClause(casePattern, statSeq) ← caseClauses) {
            if (!first && hiddenPredecessors(casePattern.firstToken).containsNewline)
              formatResult = formatResult.before(caseClause.firstToken, formatterState.currentIndentLevelInstruction)
            val arrowInstruction = PlaceAtColumn(formatterState.indentLevel, largestCasePatternLength + 1)
            formatResult ++= formatCaseClause(caseClause, Some(arrowInstruction))
            first = false
          }
        case Right(caseClause) ⇒
          if (!first && hiddenPredecessors(caseClause.firstToken).containsNewline)
            formatResult = formatResult.before(caseClause.firstToken, formatterState.currentIndentLevelInstruction)
          formatResult ++= formatCaseClause(caseClause)
      }
      first = false
    }
    formatResult
  }

  private def groupClauses(caseClausesAstNode: CaseClauses): List[Either[ConsecutiveSingleLineCaseClauses, CaseClause]] = {
    val clausesAreMultiline = containsNewline(caseClausesAstNode) || hiddenPredecessors(caseClausesAstNode.firstToken).containsNewline

    def groupClauses(caseClauses: List[CaseClause], first: Boolean): List[Either[ConsecutiveSingleLineCaseClauses, CaseClause]] =
      caseClauses match {
        case Nil ⇒ Nil
        case (caseClause@CaseClause(casePattern, statSeq)) :: otherClauses ⇒
          val otherClausesGrouped = groupClauses(otherClauses, first = false)

          val casePatternSource = getSource(casePattern)
          val casePatternFormatResult = formatCasePattern(casePattern)(FormatterState(indentLevel = 0))
          val offset = casePattern.firstToken.startIndex
          val edits = writeTokens(casePatternSource, casePattern.tokens, casePatternFormatResult, offset)
          val formattedText = TextEditProcessor.runEdits(casePatternSource, edits)

          val newlineBeforeClause = hiddenPredecessors(caseClause.firstToken).containsNewline
          val clauseBodyIsMultiline = containsNewline(statSeq) || statSeq.firstTokenOption.exists(hiddenPredecessors(_).containsNewline)
          if (formattedText.contains('\n') || (first && !clausesAreMultiline) || (!first && !newlineBeforeClause) || clauseBodyIsMultiline)
            Right(caseClause) :: otherClausesGrouped
          else {
            val arrowAdjust = (if (formattingPreferences(RewriteArrowSymbols)) 1 else casePattern.arrow.length) + 1
            val casePatternLength = formattedText.length - arrowAdjust
            otherClausesGrouped match {
              case Left(consecutiveSingleLineCaseClauses) :: otherGroups ⇒
                Left(consecutiveSingleLineCaseClauses.prepend(caseClause, casePatternLength)) :: otherGroups
              case _ ⇒
                Left(ConsecutiveSingleLineCaseClauses(caseClause :: Nil, casePatternLength)) :: otherClausesGrouped
            }
          }
      }
    groupClauses(caseClausesAstNode.caseClauses, first = true)
  }

  private case class ConsecutiveSingleLineCaseClauses(clauses: List[CaseClause], largestCasePatternLength: Int) {
    def prepend(clause: CaseClause, length: Int) =
      ConsecutiveSingleLineCaseClauses(clause :: clauses, scala.math.max(length, largestCasePatternLength))
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
    val singleBlockExpr = cond(statSeq.firstStatOpt) { case Some(Expr(List(BlockExpr(_, _, _)))) ⇒ true } && statSeq.otherStats.isEmpty
    val indentBlock = statSeq.firstTokenOption.isDefined && hiddenPredecessors(statSeq.firstToken).containsNewline || (containsNewline(statSeq) && !singleBlockExpr)
    if (indentBlock)
      formatResult = formatResult.before(statSeq.firstToken, formatterState.nextIndentLevelInstruction)

    val stateForStatSeq = if (singleBlockExpr) formatterState else formatterState.indent
    formatResult ++= format(statSeq)(stateForStatSeq)
    formatResult
  }

}
