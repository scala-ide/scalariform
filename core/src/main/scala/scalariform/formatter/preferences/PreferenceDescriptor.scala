package scalariform.formatter.preferences

sealed trait PreferenceType[T] {
  def cast(preference: PreferenceDescriptor[_]): PreferenceDescriptor[T] = preference.asInstanceOf[PreferenceDescriptor[T]]
}

case object BooleanPreference extends PreferenceType[Boolean]

case class IntegerPreference(min: Int, max: Int) extends PreferenceType[Int]

trait PreferenceDescriptor[T] {

  val key: String

  val description: String

  val preferenceType: PreferenceType[T]

  val defaultValue: T

}

abstract trait BooleanPreferenceDescriptor extends PreferenceDescriptor[Boolean] {
  val preferenceType = BooleanPreference
}

object AllPreferences {
  val preferences: List[PreferenceDescriptor[_]] = List(RewriteArrowSymbols, IndentSpaces, SpaceBeforeColon, CompactStringConcatenation,
    PreserveSpaceBeforeArguments, AlignParameters, DoubleIndentClassDeclaration)

  val preferencesByKey: Map[String, PreferenceDescriptor[_]] = {
    var map: Map[String, PreferenceDescriptor[_]] = Map()
    for (preference ← preferences)
      map = map + (preference.key -> preference)
    map
  }

}


case object RewriteArrowSymbols extends BooleanPreferenceDescriptor {
  val key = "rewriteArrowSymbols"
  val description = "Replace arrow tokens with unicode equivalents: => with ⇒, and <- with ←"
  val defaultValue = false
}

case object IndentSpaces extends PreferenceDescriptor[Int] {
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

case object DoubleIndentClassDeclaration extends BooleanPreferenceDescriptor {
  val key = "doubleIndentClassDeclaration"
  val description = "Double indent either a class's parameters or its inheritance list"
  val defaultValue = false
}

case object FormatXml extends BooleanPreferenceDescriptor {
  val key = "formatXml"
  val description = "Format XML literals"
  val defaultValue = false
}

