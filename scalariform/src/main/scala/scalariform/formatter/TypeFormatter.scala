package scalariform.formatter

import scalariform.lexer.Tokens._
import scalariform.lexer.Token
import scalariform.parser._
import scalariform.utils._
import scalariform.lexer.Tokens
import scalariform.formatter.preferences._
trait TypeFormatter { self: HasFormattingPreferences with AnnotationFormatter with ExprFormatter with ScalaFormatter ⇒

  def format(type_ : Type)(implicit formatterState: FormatterState): FormatResult = format(type_.contents)

  def format(typeElements: List[TypeElement])(implicit formatterState: FormatterState): FormatResult = {
    var formatResult = format(typeElements.head)
    for ((previousElement, element) ← Utils.stagger(typeElements)) {
      if (previousElement.isInstanceOf[Annotation] ||
        previousElement.isInstanceOf[Refinement] ||
        previousElement.isInstanceOf[InfixTypeConstructor] ||
        element.isInstanceOf[Refinement] ||
        element.isInstanceOf[InfixTypeConstructor])
        formatResult = formatResult.before(element.firstToken, CompactEnsuringGap)
      else if (element.isInstanceOf[Annotation]) {
        val instruction =
          previousElement match {
            case GeneralTokens(tokens) if tokens.last.getType == Tokens.LBRACKET ⇒ Compact
            case _ ⇒ CompactEnsuringGap
          }
        formatResult = formatResult.before(element.firstToken, instruction)
      } else if (previousElement.isInstanceOf[VarianceTypeElement])
        formatResult = formatResult.before(element.firstToken, Compact)
      else if (element.isInstanceOf[VarargsTypeElement])
        formatResult = formatResult.before(element.firstToken, Compact)
      //      else if (previousElement.isInstanceOf[CallByNameTypeElement])
      //	formatResult = formatResult.before(element.firstToken, Compact)
      formatResult ++= format(element)
    }
    formatResult
  }

  private def format(typeElement: TypeElement)(implicit formatterState: FormatterState): FormatResult = {
    typeElement match {
      case type_ @Type(_) ⇒ format(type_)
      case refinement@Refinement(_, _, _) ⇒ format(refinement)
      case annotation@Annotation(_, _, _, _) ⇒ format(annotation)
      case TypeParamClause(contents) ⇒ format(contents)
      case VarianceTypeElement(id) ⇒ NoFormatResult
      case VarargsTypeElement(star) ⇒ NoFormatResult
      case _ ⇒ NoFormatResult
    }
  }

  private def format(refinement: Refinement)(implicit formatterState: FormatterState): FormatResult = {
    val Refinement(lbrace: Token, statSeq: StatSeq, rbrace: Token) = refinement
    val dummyBlock = BlockExpr(lbrace, Right(statSeq), rbrace)
    format(dummyBlock, indent = true)
  }

}

