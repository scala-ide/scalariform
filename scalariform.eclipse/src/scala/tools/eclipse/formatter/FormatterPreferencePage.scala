package scala.tools.eclipse.formatter
import org.eclipse.ui._
import org.eclipse.jface.preference._
import scalariform.formatter._
import scalariform.formatter.preferences._

class FormatterPreferencePage extends FieldEditorPreferencePage with IWorkbenchPreferencePage {
  import FormatterPreferencePage._
  setPreferenceStore(ScalaFormatterPlugin.getDefault.getPreferenceStore)

  def init(workbench: IWorkbench) {}

  override def createFieldEditors() {

    for (preference ← AllPreferences.preferences) {
      val preferenceType = preference.preferenceType
      preferenceType match {
        case BooleanPreference ⇒
          val field = new BooleanFieldEditor(prefix + preference.key, preference.description, org.eclipse.swt.SWT.NONE, getFieldEditorParent())
          addField(field)
        case IntegerPreference(min, max) ⇒
          val field = new IntegerFieldEditor(prefix + preference.key, preference.description, getFieldEditorParent())
          field.setValidRange(min, max)
          addField(field)
      }
    }
  }
}

object FormatterPreferencePage {

  private val prefix = "scalariform."

  def getPreferences() = {
    val preferenceStore = ScalaFormatterPlugin.getDefault.getPreferenceStore
    var preferences: IFormattingPreferences = FormattingPreferences()

    for (preference ← AllPreferences.preferences) {
      preference.preferenceType match {
        case prefType@BooleanPreference ⇒
          preferences = preferences.setPreference(prefType.cast(preference), preferenceStore.getBoolean(prefix + preference.key))
        case prefType@IntegerPreference(_, _) ⇒
          preferences = preferences.setPreference(prefType.cast(preference), preferenceStore.getInt(prefix + preference.key))
      }
    }
    preferences
  }

  def initialiseDefaultPreferences(preferenceStore: IPreferenceStore): Unit =
    for {
      preference ← AllPreferences.preferences
      val key = prefix + preference.key
    } preference.preferenceType match {
      case prefType@BooleanPreference ⇒
        preferenceStore.setDefault(key, prefType.cast(preference).defaultValue)
      case prefType@IntegerPreference(_, _) ⇒
        preferenceStore.setDefault(key, prefType.cast(preference).defaultValue)
    }
}
