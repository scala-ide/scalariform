package scala.tools.eclipse.formatter

import org.eclipse.ui.plugin.AbstractUIPlugin
import org.eclipse.jface.preference._

object ScalaFormatterPlugin {

  private var plugin: Option[ScalaFormatterPlugin] = None

  def getDefault: ScalaFormatterPlugin = plugin.get

}

class ScalaFormatterPlugin extends AbstractUIPlugin {

  ScalaFormatterPlugin.plugin = Some(this)

  override def initializeDefaultPreferences(preferenceStore: IPreferenceStore) =
    FormatterPreferencePage.initialiseDefaultPreferences(preferenceStore)

}

