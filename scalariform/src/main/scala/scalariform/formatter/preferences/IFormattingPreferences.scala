package scalariform.formatter.preferences

trait IFormattingPreferences {

  def apply[T](preference: PreferenceDescriptor[T]): T

  def setPreference[T](preference: PreferenceDescriptor[T], value: T): IFormattingPreferences

  def preferencesMap: Map[PreferenceDescriptor[_], Any]

  def indentStyle: IndentStyle

}

abstract sealed class IndentStyle {
  def indent(n: Int): String
  protected def repeat(s: String, n: Int) = 1 to n map { _ ⇒ s } mkString
}

case object Tabs extends IndentStyle {
  def indent(indentLevel: Int) = repeat("\t", indentLevel)
}

case class Spaces(n: Int) extends IndentStyle {

  def indent(indentLevel: Int) = repeat(repeat(" ", n), indentLevel)

  def length(indentLevel: Int) = indent(indentLevel).length

}

class FormattingPreferences(val preferencesMap: Map[PreferenceDescriptor[_], Any]) extends IFormattingPreferences {

  def apply[T](preference: PreferenceDescriptor[T]): T = preferencesMap.get(preference) map { _.asInstanceOf[T] } getOrElse preference.defaultValue

  def setPreference[T](preference: PreferenceDescriptor[T], value: T) = new FormattingPreferences(preferencesMap + (preference -> value))

  override def toString = getClass.getSimpleName + "(" + preferencesMap + ")"

  val indentStyle = if (this(IndentWithTabs)) Tabs else Spaces(this(IndentSpaces))
}

case object FormattingPreferences extends FormattingPreferences(Map()) {

  def apply() = new FormattingPreferences(Map())

}

trait HasFormattingPreferences {

  val formattingPreferences: IFormattingPreferences

}
