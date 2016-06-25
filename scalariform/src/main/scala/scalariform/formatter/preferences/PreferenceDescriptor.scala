package scalariform.formatter.preferences

sealed trait PreferenceType[T] {

  def cast(preference: PreferenceDescriptor[_]): PreferenceDescriptor[T] = preference.asInstanceOf[PreferenceDescriptor[T]]

  def parseValue(s: String): Either[String, T]

}

/** Trinary state setting for preference which can be enforced two ways or disabled. */
sealed trait Intent extends Product with Serializable

/** Preserve the formatting choice made at the site. */
case object Preserve extends Intent

/** Force the formatting choice in the preference name. */
case object Force extends Intent

/** Prevent the formatting choice in the preference name. */
case object Prevent extends Intent

case object IntentPreference extends PreferenceType[Intent] {

  def parseValue(s: String) =
    s.toLowerCase match {
      case "preserve" ⇒ Right(Preserve)
      case "force"    ⇒ Right(Force)
      case "prevent"  ⇒ Right(Prevent)
      case _          ⇒ Left("Could not parse as intent value: " + s)
    }
}

case object BooleanPreference extends PreferenceType[Boolean] {

  def parseValue(s: String) =
    s.toLowerCase match {
      case "true"  ⇒ Right(true)
      case "false" ⇒ Right(false)
      case _       ⇒ Left("Could not parse as boolean value: " + s)
    }

}

case class IntegerPreference(min: Int, max: Int) extends PreferenceType[Int] {

  require(min <= max)

  def parseValue(s: String) =
    try {
      val n = Integer.parseInt(s)
      if (n < min)
        Left(n + " is below minimum of " + min)
      else if (n > max)
        Left(n + " is above maximum of " + max)
      else
        Right(n)
    } catch {
      case e: NumberFormatException ⇒ Left("Could not parse as integer: " + s)
    }
}

trait PreferenceDescriptor[T] {

  val key: String

  val description: String

  val preferenceType: PreferenceType[T]

  val defaultValue: T

}

trait IntentPreferenceDescriptor extends PreferenceDescriptor[Intent] {

  val preferenceType = IntentPreference

}

trait BooleanPreferenceDescriptor extends PreferenceDescriptor[Boolean] {

  val preferenceType = BooleanPreference

}

trait IntegerPreferenceDescriptor extends PreferenceDescriptor[Int] {

}

object AllPreferences {
  val preferences: List[PreferenceDescriptor[_]] = List(
    RewriteArrowSymbols, IndentSpaces, SpaceBeforeColon, SpaceBeforeContextColon, CompactStringConcatenation,
    PreserveSpaceBeforeArguments, AlignParameters, FirstParameterOnNewline, AlignArguments, FirstArgumentOnNewline,
    DoubleIndentClassDeclaration, FormatXml, IndentPackageBlocks, AlignSingleLineCaseStatements,
    AlignSingleLineCaseStatements.MaxArrowIndent, IndentLocalDefs, PreserveDanglingCloseParenthesis, DanglingCloseParenthesis,
    SpaceInsideParentheses, SpaceInsideBrackets, SpacesWithinPatternBinders, MultilineScaladocCommentsStartOnFirstLine, IndentWithTabs,
    CompactControlReadability, PlaceScaladocAsterisksBeneathSecondAsterisk, DoubleIndentMethodDeclaration, SpacesAroundMultiImports,
    NewlineAtEndOfFile
  )

  val preferencesByKey: Map[String, PreferenceDescriptor[_]] =
    preferences.foldLeft(Map.empty[String, PreferenceDescriptor[_]]) {
      case (m, preference) ⇒ m + (preference.key → preference)
    }
}

case object RewriteArrowSymbols extends BooleanPreferenceDescriptor {
  val key = "rewriteArrowSymbols"
  val description = "Replace arrow tokens with unicode equivalents: => with ⇒, and <- with ←"
  val defaultValue = false
}

case object IndentSpaces extends IntegerPreferenceDescriptor {
  val key = "indentSpaces"
  val description = "Number of spaces to use for indentation"
  val preferenceType = IntegerPreference(1, 10)
  val defaultValue = 2
}

case object SpaceBeforeColon extends BooleanPreferenceDescriptor {
  val key = "spaceBeforeColon"
  val description = "Add a space before colons"
  val defaultValue = false
}

case object SpaceBeforeContextColon extends BooleanPreferenceDescriptor {
  val key = "spaceBeforeContextColon"
  val description = "Add a space before colons in context bounds"
  val defaultValue = false
}

case object CompactStringConcatenation extends BooleanPreferenceDescriptor {
  val key = "compactStringConcatenation"
  val description = "Omit spaces when formatting a '+' operator on String literals"
  val defaultValue = false
}

case object PreserveSpaceBeforeArguments extends BooleanPreferenceDescriptor {
  val key = "preserveSpaceBeforeArguments"
  val description = "Preserve a space before a parenthesis argument"
  val defaultValue = false
}

case object AlignParameters extends BooleanPreferenceDescriptor {
  val key = "alignParameters"
  val description = "Align parameters on different lines in the same column"
  val defaultValue = false
}

case object FirstParameterOnNewline extends IntentPreferenceDescriptor {
  val key = "firstParameterOnNewline"
  val description = "Places the first parameter in function or class definitions on a new line"
  val defaultValue = Force
}

case object AlignArguments extends BooleanPreferenceDescriptor {
  val key = "alignArguments"
  val description = "Align method arguments on different lines in the same column"
  val defaultValue = false
}

case object FirstArgumentOnNewline extends IntentPreferenceDescriptor {
  val key = "firstArgumentOnNewline"
  val description = "Places the first argument to functions calls on a new line"
  val defaultValue = Force
}

case object DoubleIndentClassDeclaration extends BooleanPreferenceDescriptor {
  val key = "doubleIndentClassDeclaration"
  val description = "Double indent either a class's parameters or its inheritance list"
  val defaultValue = false
}

case object DoubleIndentMethodDeclaration extends BooleanPreferenceDescriptor {
  val key = "doubleIndentMethodDeclaration"
  val description = "Double indent a method's parameters, if they span multiple lines"
  val defaultValue = false
}

case object FormatXml extends BooleanPreferenceDescriptor {
  val key = "formatXml"
  val description = "Format XML literals"
  val defaultValue = true
}

case object IndentPackageBlocks extends BooleanPreferenceDescriptor {
  val key = "indentPackageBlocks"
  val description = "Indent package blocks"
  val defaultValue = true
}

case object AlignSingleLineCaseStatements extends BooleanPreferenceDescriptor {
  val key = "alignSingleLineCaseStatements"
  val description = "Align the arrows of consecutive single-line case statements"
  val defaultValue = false

  case object MaxArrowIndent extends IntegerPreferenceDescriptor {
    val key = "alignSingleLineCaseStatements.maxArrowIndent"
    val description = "Maximum number of spaces inserted before an arrow to align case statements"
    val preferenceType = IntegerPreference(1, 100)
    val defaultValue = 40
  }

}

case object IndentLocalDefs extends BooleanPreferenceDescriptor {
  val key = "indentLocalDefs"
  val description = "Indent local defs an extra level"
  val defaultValue = false
}

@deprecated("This has been dropped in favor of DanglingCloseParenthesis.", since = "0.1.5")
case object PreserveDanglingCloseParenthesis extends BooleanPreferenceDescriptor {
  val key = "preserveDanglingCloseParenthesis"
  val description = "Allow a newline before a ')' in an argument expression"
  val defaultValue = false
}

case object DanglingCloseParenthesis extends IntentPreferenceDescriptor {
  val key = "danglingCloseParenthesis"
  val description = "Put a newline before a ')' in an argument expression"
  val defaultValue = Prevent
}

case object SpaceInsideParentheses extends BooleanPreferenceDescriptor {
  val key = "spaceInsideParentheses"
  val description = "Require a space after '(' and before ')'"
  val defaultValue = false
}

case object SpaceInsideBrackets extends BooleanPreferenceDescriptor {
  val key = "spaceInsideBrackets"
  val description = "Require a space after '[' and before ']'"
  val defaultValue = false
}

case object SpacesWithinPatternBinders extends BooleanPreferenceDescriptor {
  val key = "spacesWithinPatternBinders"
  val description = "Add a space around the @ token in pattern binders"
  val defaultValue = true
}

case object MultilineScaladocCommentsStartOnFirstLine extends BooleanPreferenceDescriptor {
  val key = "multilineScaladocCommentsStartOnFirstLine"
  val description = "Start multiline Scaladoc comment body on same line as the opening '/**' "
  val defaultValue = false
}

case object IndentWithTabs extends BooleanPreferenceDescriptor {
  val key = "indentWithTabs"
  val description = "Use a tab character for indentation"
  val defaultValue = false
}

case object CompactControlReadability extends BooleanPreferenceDescriptor {
  val key = "compactControlReadability"
  val description = "Enable Compact Control Readability style"
  val defaultValue = false
}

case object PlaceScaladocAsterisksBeneathSecondAsterisk extends BooleanPreferenceDescriptor {
  val key = "placeScaladocAsterisksBeneathSecondAsterisk"
  val description = "Place Scaladoc asterisks beneath the second asterisk in the opening '/**', as opposed to the first"
  val defaultValue = false
}

case object SpacesAroundMultiImports extends BooleanPreferenceDescriptor {
  val key = "spacesAroundMultiImports"
  val description = "Place spaces around multi imports (import a.{ b, c, d }"
  val defaultValue = true
}

case object NewlineAtEndOfFile extends BooleanPreferenceDescriptor {
  val key = "newlineAtEndOfFile"
  val description = "Add a newline at the end of all files"
  val defaultValue = false
}
