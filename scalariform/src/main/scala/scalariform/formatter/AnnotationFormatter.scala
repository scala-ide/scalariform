package scalariform.formatter

import scalariform.parser._
import scalariform.utils._
import scalariform.lexer._
import scalariform.formatter.preferences._
trait AnnotationFormatter { self: HasFormattingPreferences with TypeFormatter with ExprFormatter ⇒

  def format(annotation: Annotation)(implicit formatterState: FormatterState): FormatResult = {
    val Annotation(atToken: Token, annotationType: Type, argumentExprss: List[ArgumentExprs], newlineOption: Option[Token]) = annotation
    var formatResult: FormatResult = NoFormatResult

    formatResult ++= format(annotationType)
    for (argumentExprs ← argumentExprss)
      formatResult ++= format(argumentExprs)
    for (newline ← newlineOption)
      formatResult = formatResult.formatNewline(newline, Compact) // TODO: rethink
    formatResult
  }
}

