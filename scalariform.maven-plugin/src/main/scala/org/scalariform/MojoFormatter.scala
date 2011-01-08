package org.scalariform

import scalariform.formatter.preferences._
import scalariform.formatter.ScalaFormatter
import scalariform.parser.ScalaParserException
import scalariform.utils.Utils.writeText

import java.io.{ File, FilenameFilter, FileFilter }

import scala.collection.JavaConversions._

import scala.io.Source

/**
 * Goal which formats scala source files
 *
 * @goal format
 * 
 * @phase process-sources
 */
object MojoFormatter {

  val scalaFilter = new FilenameFilter {
    def accept(dir : File, name : String)  : Boolean = name.endsWith(".scala")
  }

  val dirFilter = new FileFilter() {
    def accept(dir : File ) = dir.isDirectory
  }

  private def findScalaFiles(dirpath : String) : List[File] = {
     def findScalaFilesByFile(path : File, list : List[File]) : List[File] = {
       val runningList = path.listFiles(scalaFilter).toList ::: list
       path.listFiles(dirFilter).foldLeft(runningList) { (sum,dir) =>
         findScalaFilesByFile(dir, sum)
       }
     }     
     findScalaFilesByFile(new File(dirpath), Nil)
  }

  def format(path : String, 
             alignParameters : Boolean,
             doubleIndentClassDeclaration : Boolean,
             compactStringConcatenation : Boolean,
             preserveSpaceBeforeArguments : Boolean,
             rewriteArrowSymbols : Boolean,
             spaceBeforeColon : Boolean,
             indentSpaces : Int) {

    val preferences = FormattingPreferences()
      .setPreference(AlignParameters, alignParameters)
      .setPreference(DoubleIndentClassDeclaration, doubleIndentClassDeclaration)
      .setPreference(CompactStringConcatenation, compactStringConcatenation)
      .setPreference(PreserveSpaceBeforeArguments, preserveSpaceBeforeArguments)
      .setPreference(RewriteArrowSymbols, rewriteArrowSymbols)
      .setPreference(SpaceBeforeColon, spaceBeforeColon)
      .setPreference(IndentSpaces, indentSpaces)
    
    findScalaFiles(path).foreach { file =>
      val original = Source.fromFile(file).mkString
      writeText(file, ScalaFormatter.format(original, preferences))
    }
  }

}
