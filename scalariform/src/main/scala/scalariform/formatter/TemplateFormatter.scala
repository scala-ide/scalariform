package scalariform.formatter

import scalariform.lexer.Token

import scalariform.parser._
import scalariform.formatter.preferences._

trait TemplateFormatter { self: HasFormattingPreferences with AnnotationFormatter with HasHiddenTokenInfo with TypeFormatter with ExprFormatter with ScalaFormatter ⇒

  def format(tmplDef: TmplDef)(implicit formatterState: FormatterState): FormatResult = {
    val TmplDef(_, _, typeParamClauseOpt, annotations, accessModifierOpt, paramClausesOpt, templateInheritanceSectionOpt, templateBodyOption) = tmplDef
    var formatResult: FormatResult = NoFormatResult
    for (typeParamClause ← typeParamClauseOpt)
      formatResult ++= format(typeParamClause.contents)

    for (annotation ← annotations) {
      formatResult = formatResult.before(annotation.firstToken, CompactEnsuringGap)
      formatResult ++= format(annotation)
    }

    for {
      accessModifier ← accessModifierOpt
      astNode ← (paramClausesOpt orElse templateInheritanceSectionOpt orElse templateBodyOption)
      firstToken ← astNode.firstTokenOption
    } formatResult = formatResult.formatNewlineOrOrdinary(firstToken, CompactEnsuringGap)

    for {
      paramClauses ← paramClausesOpt
      firstToken ← paramClauses.firstTokenOption
    } {
      if (annotations.size > 0)
        formatResult = formatResult.formatNewlineOrOrdinary(firstToken, CompactEnsuringGap)
      val doubleIndentParams = (formattingPreferences(DoubleIndentConstructorArguments) &&
        !templateInheritanceSectionOpt.exists { section ⇒
          containsNewline(section) || hiddenPredecessors(section.firstToken).containsNewline
        } &&
        templateBodyOption.exists(containsNewline(_))) || formattingPreferences(DoubleIndentConstructorArguments)
      formatResult ++= formatParamClauses(paramClauses, doubleIndentParams)
    }
    for (TemplateInheritanceSection(extendsOrSubtype, earlyDefsOpt, templateParentsOpt) ← templateInheritanceSectionOpt) {
      val doubleIndentTemplateInheritance = !formattingPreferences(DoubleIndentConstructorArguments) &&
        formattingPreferences(DoubleIndentClassDeclaration) &&
        (templateBodyOption.exists(containsNewline(_)) || paramClausesOpt.exists(containsNewline(_)))
      val inheritanceIndent = if (doubleIndentTemplateInheritance) 2 else 1
      var currentFormatterState = formatterState
      if (hiddenPredecessors(extendsOrSubtype).containsNewline) {
        currentFormatterState = formatterState.indent(inheritanceIndent)
        formatResult = formatResult.before(extendsOrSubtype, currentFormatterState.currentIndentLevelInstruction)
      }
      for (EarlyDefs(earlyBody: TemplateBody, withOpt) ← earlyDefsOpt)
        formatResult ++= format(earlyBody)(currentFormatterState)

      for (templateParents ← templateParentsOpt) {
        val TemplateParents((type1: Type, argumentExprss: List[ArgumentExprs]), withTypes: List[(Token, Type, List[ArgumentExprs])]) = templateParents

        formatResult ++= format(type1)(currentFormatterState)
        for (argumentExprs ← argumentExprss)
          formatResult ++= format(argumentExprs)(currentFormatterState)._1

        for ((withToken, type_, argumentExprss2) ← withTypes) {
          if (hiddenPredecessors(withToken).containsNewline) {
            currentFormatterState = formatterState.indent(inheritanceIndent)
            formatResult = formatResult.before(withToken, currentFormatterState.currentIndentLevelInstruction)
          }
          formatResult ++= format(type_)(currentFormatterState)
          for (argumentExprs2 ← argumentExprss2)
            formatResult ++= format(argumentExprs2)(currentFormatterState)._1
        }
      }
    }

    for (templateBody ← templateBodyOption)
      formatResult ++= format(templateBody)

    formatResult
  }

  // TODO: Copy and pasted below
  private def format(templateBody: TemplateBody)(implicit formatterState: FormatterState): FormatResult = {
    val TemplateBody(newlineOpt, lbrace, statSeq, rbrace) = templateBody
    var formatResult: FormatResult = NoFormatResult
    newlineOpt match {
      case Some(newline) ⇒
        formatResult = formatResult.formatNewline(newline, CompactEnsuringGap)
      case None ⇒
    }

    val dummyBlock = BlockExpr(lbrace, Right(statSeq), rbrace)
    formatResult ++= format(dummyBlock, indent = true)
    formatResult
  }

  def format(template: Template)(implicit formatterState: FormatterState): FormatResult = {
    val Template(earlyDefsOpt, templateParentsOpt, templateBodyOpt) = template
    var formatResult: FormatResult = NoFormatResult

    for (EarlyDefs(earlyBody, withOpt) ← earlyDefsOpt)
      formatResult ++= format(earlyBody)

    for (templateParents ← templateParentsOpt)
      formatResult ++= format(templateParents)

    // TODO: Copy and paste from above
    for (templateBody @ TemplateBody(newlineOpt, lbrace, statSeq, rbrace) ← templateBodyOpt) {
      newlineOpt match {
        case Some(newline) ⇒
          formatResult = formatResult.formatNewline(newline, CompactEnsuringGap)
        case None ⇒
          formatResult = formatResult.before(lbrace, CompactEnsuringGap)
      }

      val dummyBlock = BlockExpr(lbrace, Right(statSeq), rbrace)
      formatResult ++= format(dummyBlock, indent = true)
    }

    formatResult
  }

  private def format(templateParents: TemplateParents)(implicit formatterState: FormatterState): FormatResult = {
    var formatResult: FormatResult = NoFormatResult
    val TemplateParents((type1, argumentExprss), withTypes) = templateParents
    formatResult ++= format(type1)
    for (argumentExprs ← argumentExprss)
      formatResult ++= format(argumentExprs)._1

    // TODO: Unify with TmplDef code

    val currentFormatterState = formatterState
    for ((withToken, type_, argumentExprss2) ← withTypes) {
      formatResult ++= format(type_)(currentFormatterState)
      for (argumentExprs2 ← argumentExprss2)
        formatResult ++= format(argumentExprs2)(currentFormatterState)._1
    }

    formatResult
  }

}
