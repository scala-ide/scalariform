package scalariform.gui

object Utils {

  def onSwingThread(proc: ⇒ Unit) = javax.swing.SwingUtilities.invokeLater(new Runnable() { def run() = proc })

  import javax.swing.JTree
  import javax.swing.tree._

  def expandAll(tree: JTree) {
    val root = tree.getModel().getRoot()
    expandAll(tree, new TreePath(root))
  }

  private def expandAll(tree: JTree, parent: TreePath) {
    val node = parent.getLastPathComponent()
    val model = tree.getModel
    val children = 0 until model.getChildCount(node) map { model.getChild(node, _) }
    for (child ← children) {
      val path = parent.pathByAddingChild(child)
      expandAll(tree, path)
    }
    tree.expandPath(parent)
  }

}
