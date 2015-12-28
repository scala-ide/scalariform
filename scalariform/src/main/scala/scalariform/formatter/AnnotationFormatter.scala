package scalariform.formatter

import scalariform.parser._
import scalariform.formatter.preferences._

trait AnnotationFormatter { self: HasFormattingPreferences with TypeFormatter with ExprFormatter ⇒

  def format(annotation: Annotation)(implicit formatterState: FormatterState): FormatResult = {
    val Annotation(_, annotationType, argumentExprss, newlineOption) = annotation
    var formatResult: FormatResult = NoFormatResult

    formatResult = formatResult.before(annotationType.firstToken, Compact)
    formatResult ++= format(annotationType)
    for (argumentExprs ← argumentExprss)
      formatResult ++= format(argumentExprs)._1
    for (newline ← newlineOption)
      formatResult = formatResult.formatNewline(newline, Compact) // TODO: rethink
    formatResult
  }

}
