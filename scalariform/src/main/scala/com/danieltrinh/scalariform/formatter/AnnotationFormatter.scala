package com.danieltrinh.scalariform.formatter

import com.danieltrinh.scalariform.parser._
import com.danieltrinh.scalariform.utils._
import com.danieltrinh.scalariform.lexer._
import com.danieltrinh.scalariform.formatter.preferences._

trait AnnotationFormatter { self: HasFormattingPreferences with TypeFormatter with ExprFormatter ⇒

  def format(annotation: Annotation)(implicit formatterState: FormatterState): FormatResult = {
    val Annotation(atToken: Token, annotationType: Type, argumentExprss: List[ArgumentExprs], newlineOption: Option[Token]) = annotation
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

