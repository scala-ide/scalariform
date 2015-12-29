package scalariform.utils

import java.io.FileOutputStream
import java.io.FileInputStream
import java.io.IOException

object Utils {

  implicit def string2PimpedString(s: String) = new PimpedString(s)

  class PimpedString(s: String) {
    def toIntOpt: Option[Int] = try Some(s.toInt) catch { case _: NumberFormatException ⇒ None }
  }

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

  def groupBy[A](eq: (A, A) ⇒ Boolean, lst: List[A]): List[List[A]] =
    lst match {
      case Nil ⇒ Nil
      case (x :: xs) ⇒ {
        val (ys, zs) = xs span { eq(x, _) }
        (x :: ys) :: groupBy(eq, zs)
      }
    }

  // File ------------------

  @throws(classOf[IOException])
  def withFileInputStream[T](fileName: String)(p: FileInputStream ⇒ T): T = {
    var fis: FileInputStream = null
    try {
      fis = new FileInputStream(fileName)
      p(fis)
    } finally
      if (fis != null)
        fis.close()
  }

  @throws(classOf[IOException])
  def withFileOutputStream[T](fileName: String)(p: FileOutputStream ⇒ T): T = {
    var fis: FileOutputStream = null
    try {
      fis = new FileOutputStream(fileName)
      p(fis)
    } finally
      if (fis != null)
        fis.close()
  }

  def digit2int(ch: Char, base: Int): Int =
    if ('0' <= ch && ch <= '9' && ch < '0' + base)
      ch - '0'
    else if ('A' <= ch && ch < 'A' + base - 10)
      ch - 'A' + 10
    else if ('a' <= ch && ch < 'a' + base - 10)
      ch - 'a' + 10
    else
      -1

  def deleteRange(text: String, range: Range): String =
    replaceRange(text, range, replacement = "")

  def replaceRange(text: String, range: Range, replacement: String): String =
    text.take(range.offset) + replacement + text.drop(range.offset + range.length)

}
