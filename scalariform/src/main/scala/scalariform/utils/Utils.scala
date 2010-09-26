package scalariform.utils

object Utils {

  def asInstanceOf[T](o: Any) = if (o.isInstanceOf[T]) Some(o.asInstanceOf[T]) else None

  def checkNotNull[T](item: T): T = { require(item != null); item }

  implicit def boolean2ImpliesWrapper(b: Boolean): ImpliesWrapper = new ImpliesWrapper(b)

  class ImpliesWrapper(b: Boolean) {
    def implies(b2: ⇒ Boolean) = if (!b) true else b2
  }

  def stagger[T](iterable: Iterable[T]) = iterable zip iterable.tail

  def pairWithPrevious[T](iterable: Iterable[T]): List[(Option[T], T)] = {
    if (iterable.isEmpty)
      Nil
    else {
      val previous = None :: (iterable.init map Some[T]).toList
      previous zip iterable
    }
  }

  def withPreviousAndNext[T](iterable: Iterable[T]): List[(Option[T], T, Option[T])] = {
    if (iterable.isEmpty)
      Nil
    else {
      val previous = None :: (iterable.init map Some[T]).toList
      val next = (iterable.tail map Some[T]).toList ::: List(None)
      previous zip iterable zip next map { case ((a, b), c) ⇒ (a, b, c) }
    }
  }

  def enumerate[T](xs: Iterable[T]): List[(Int, T)] = (0 to xs.size) zip xs toList

  import scala.reflect.Manifest
  implicit def any2optionable(x: AnyRef) = new {
    def matchInstance[B](implicit m: Manifest[B]): Option[B] =
      if (Manifest.singleType(x) <:< m)
        Some(x.asInstanceOf[B])
      else
        None
  }

  def groupBy[A](eq: (A, A) ⇒ Boolean, lst: List[A]): List[List[A]] =
    lst match {
      case Nil ⇒ Nil
      case (x :: xs) ⇒ {
        val (ys, zs) = xs span { eq(x, _) }
        (x :: ys) :: groupBy(eq, zs)
      }
    }

  // Swing ---------------------

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

  // File ------------------

  def writeText(file: java.io.File, text: String, encodingOpt: Option[String] = None) {
    import java.io.{ OutputStreamWriter, FileOutputStream }
    val encoding = encodingOpt getOrElse (System getProperty "file.encoding")
    val writer = new OutputStreamWriter(new FileOutputStream(file), encoding)
    try {
      writer.write(text)
    } finally {
      writer.close()
    }
  }

  def time[T](s: String)(f: ⇒ T): T = {
    val start = System.currentTimeMillis
    val result = f
    val duration = System.currentTimeMillis - start
    println(s + ": " + duration + "ms")
    result
  }

}

