package scalariform.gui

import javax.swing.JFrame

object Main {

  def main(args: Array[String]) {
    Utils.onSwingThread {
      val frame = new FormatterFrame
      frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      frame.setSize(1280, 600)
      frame.setVisible(true)
    }
  }
}
