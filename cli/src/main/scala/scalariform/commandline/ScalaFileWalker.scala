package scalariform.commandline

import java.io.File
import java.util.{ ArrayList, Collection }
import scala.collection.JavaConversions._

import org.apache.commons.io._
import org.apache.commons.io.filefilter._

object ScalaFileWalker extends DirectoryWalker(TrueFileFilter.INSTANCE, FileFilterUtils.suffixFileFilter(".scala"), -1) {

  def findScalaFiles(path: String): List[File] = findScalaFiles(new File(path))

  def findScalaFiles(path: File): List[File] = {
    val results = new ArrayList[File]
    walk(path, results)
    results.toList
  }

  override protected def handleFile(file: File, depth: Int, results: Collection[_]) {
    val castResults = results.asInstanceOf[Collection[File]]
    castResults.add(file)
  }

}
