package scalariform.formatter.preferences

import java.util.Properties
import scala.collection.JavaConverters._
import scalariform.utils.Utils._
import java.io.IOException

object PreferencesImporterExporter {

  /**
   * Pull preferences from a Properties store.
   * Errors are silently ignored. (TODO)
   */
  def getPreferences(properties: Properties): IFormattingPreferences = {

    var preferences = FormattingPreferences()

    def setPreference[T](preferenceDescriptor: PreferenceDescriptor[T], valueString: String) =
      preferenceDescriptor.preferenceType.parseValue(valueString) match {
        case Left(error)  ⇒
        case Right(value) ⇒ preferences = preferences.setPreference(preferenceDescriptor, value)
      }

    for {
      key @ (dummy: String) ← properties.propertyNames.asScala
      descriptor ← AllPreferences.preferencesByKey.get(key)
      valueString = properties.getProperty(key)
    } setPreference(descriptor, valueString)
    preferences

  }

  def asProperties(preferences: IFormattingPreferences): Properties = {
    val properties = new Properties
    for (preference ← AllPreferences.preferences)
      properties.setProperty(preference.key, preferences(preference).toString)
    properties
  }

  @throws(classOf[IOException])
  def loadPreferences(path: String): IFormattingPreferences = {
    val properties = new Properties
    withFileInputStream(path) { stream ⇒
      properties.load(stream)
    }
    getPreferences(properties)
  }

  @throws(classOf[IOException])
  def savePreferences(path: String, preferences: IFormattingPreferences) {
    val properties = asProperties(preferences)
    withFileOutputStream(path) { stream ⇒
      properties.store(stream, "Scalariform formatter preferences")
    }
  }

}
