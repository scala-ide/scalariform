package scalariform.commandline

import java.io.File
import java.util

import org.apache.commons.io._
import org.apache.commons.io.filefilter._

import scala.collection.JavaConverters._

object ScalaFileWalker extends DirectoryWalker(TrueFileFilter.INSTANCE, FileFilterUtils.suffixFileFilter(".scala"), -1) {

  def findScalaFiles(path: String): List[File] = findScalaFiles(new File(path))

  def findScalaFiles(path: File): List[File] = {
    val results = new util.ArrayList[File]
    walk(path, results)
    results.asScala.toList
  }

  override protected def handleFile(file: File, depth: Int, results: util.Collection[_]): Unit = {
    val castResults = results.asInstanceOf[util.Collection[File]]
    castResults.add(file)
  }

}
