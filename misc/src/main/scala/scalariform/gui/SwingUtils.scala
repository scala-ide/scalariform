package scalariform.gui

import javax.swing.event.ListSelectionListener
import javax.swing.event.ListSelectionEvent

object SwingUtils {

  implicit def fn2ListSelectionListener(handler: ListSelectionEvent ⇒ Unit): ListSelectionListener = new ListSelectionListener() {
    def valueChanged(e: ListSelectionEvent) = handler(e)
  }

}