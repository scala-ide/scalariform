package scalariform.formatter.preferences

import java.util.Properties
import scala.collection.JavaConversions._

object PropertiesPreferences {

  /**
   * Pull preferences from a Properties store.
   * Errors are silently ignored. (TODO)
   */
  def getPreferences(properties: Properties): IFormattingPreferences = {

    var preferences = FormattingPreferences()

    def setPreference[T](preferenceDescriptor: PreferenceDescriptor[T], valueString: String) =
      preferenceDescriptor.preferenceType.parseValue(valueString) match {
        case Left(error) =>
        case Right(value) => preferences = preferences.setPreference(preferenceDescriptor, value)
      }

    for {
      key@(dummy: String) <- properties.propertyNames
      descriptor <- AllPreferences.preferencesByKey.get(key)
      valueString = properties.getProperty(key)
    } setPreference(descriptor, valueString)
    preferences

  }

  def asProperties(preferences: IFormattingPreferences): Properties = {
    val properties = new Properties
    for (preference <- AllPreferences.preferences)
      properties.setProperty(preference.key, preferences(preference).toString)
    properties
  }

}