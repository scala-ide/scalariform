package scalariform.gui

import scalariform.utils.Utils._
import javax.swing.JFrame

object Main {

  def main(args: Array[String]) {
    onSwingThread {
      val frame = new FormatterFrame
      frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      frame.setSize(1280, 600)
      frame.setVisible(true)
    }
  }

}

