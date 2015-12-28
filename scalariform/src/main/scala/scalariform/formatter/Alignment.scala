package scalariform.formatter

import scalariform.parser._
import Math._

// For now, this is just a place to store alignment related functionality.
// TOOD: refactor duplicate behavior in here
object Alignment {
  type EitherAlignableParam = Either[ConsecutiveSingleLineParams, Param]
  type EitherAlignableEqualsExpr = Either[ConsecutiveSingleLineEqualsExprs, CallExpr]
  type EitherAlignableCaseClause = Either[ConsecutiveSingleLineCaseClauses, CaseClause]

  case class ConsecutiveSingleLineParams(params: List[Param], maxSectionLengths: ParamSectionLengths, thisSectionLengths: ParamSectionLengths) {
    def prepend(param: Param, newLengths: ParamSectionLengths): ConsecutiveSingleLineParams = {
      ConsecutiveSingleLineParams(param :: params, maxSectionLengths.max(newLengths), thisSectionLengths)
    }

    // Splits the head param off, returning it and the remaining params.
    // This doesn't recalculate section lengths.
    def pop: (Option[Param], ConsecutiveSingleLineParams) = params match {
      case param :: remainingParams ⇒
        (Some(param), copy(params = remainingParams))
      case Nil ⇒
        (None, copy(params = Nil))
    }
  }

  case class ConsecutiveSingleLineEqualsExprs(
    equalsExprs:     List[EqualsExpr],
    largestIdLength: Int
  ) {

    def prepend(equalsExpr: EqualsExpr, length: Int) = {
      ConsecutiveSingleLineEqualsExprs(equalsExpr :: equalsExprs, max(length, largestIdLength))
    }
  }

  case class ConsecutiveSingleLineCaseClauses(
    clauses:                   List[CaseClause],
    largestCasePatternLength:  Int,
    smallestCasePatternLength: Int
  ) {
    def prepend(clause: CaseClause, length: Int) =
      ConsecutiveSingleLineCaseClauses(clause :: clauses, max(length, largestCasePatternLength), min(length, smallestCasePatternLength))

    def patternLengthRange = largestCasePatternLength - smallestCasePatternLength

  }

  case class ParamSectionLengths(prefixLength: Int, idLength: Int, prefixAndIdLength: Int, typeLength: Int) {
    def max(newParamSectionLength: ParamSectionLengths): ParamSectionLengths = {
      val ParamSectionLengths(newPrefixLength, newIdLength, newPrefixAndIdLength, newTypeLength) = newParamSectionLength
      ParamSectionLengths(
        math.max(prefixLength, newPrefixLength),
        math.max(idLength, newIdLength),
        math.max(prefixAndIdLength, newPrefixAndIdLength),
        math.max(typeLength, newTypeLength)
      )
    }
  }
}
