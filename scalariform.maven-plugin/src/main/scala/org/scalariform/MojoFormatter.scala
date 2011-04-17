package org.scalariform

import scalariform.formatter.preferences._
import scalariform.formatter.ScalaFormatter
import scalariform.parser.ScalaParserException
import scalariform.utils.Utils.writeText

import java.io.{ File, FilenameFilter, FileFilter }

import scala.collection.JavaConversions._

import scala.io.Source

import org.apache.maven.plugin.logging.Log

/**
 * Goal which formats scala source files
 *
 * @goal format
 *
 * @phase process-sources
 */
object MojoFormatter {

  val scalaFilter = new FilenameFilter {
    def accept(dir: File, name: String): Boolean = name.endsWith(".scala")
  }

  val dirFilter = new FileFilter() {
    def accept(dir: File) = dir.isDirectory
  }

  private def findScalaFiles(dirpath: String): List[File] = {
    def findScalaFilesByFile(path: File, list: List[File]): List[File] = {
      val runningList = path.listFiles(scalaFilter).toList ::: list
      path.listFiles(dirFilter).foldLeft(runningList) { (sum, dir) ⇒
        findScalaFilesByFile(dir, sum)
      }
    }
    findScalaFilesByFile(new File(dirpath), Nil)
  }

  def format(path: String,
             log: Log,
             alignParameters: Boolean,
             alignSingleLineCaseStatements: Boolean,
             alignSingleLineCaseStatements_maxArrowIndent: Int,
             compactStringConcatenation: Boolean,
             doubleIndentClassDeclaration: Boolean,
             formatXml: Boolean,
             indentLocalDefs: Boolean,
             indentPackageBlocks: Boolean,
             indentSpaces: Int,
             indentWithTabs: Boolean,
             multilineScaladocCommentsStartOnFirstLine: Boolean,
             preserveDanglingCloseParenthesis: Boolean,
             preserveSpaceBeforeArguments: Boolean,
             rewriteArrowSymbols: Boolean,
             spaceBeforeColon: Boolean,
             spacesInsideBrackets: Boolean,
             spacesInsideParentheses: Boolean,
             spacesWithinPatternBinders: Boolean) {

    val preferences = FormattingPreferences()
      .setPreference(AlignParameters, alignParameters)
      .setPreference(AlignSingleLineCaseStatements, alignSingleLineCaseStatements)
      .setPreference(AlignSingleLineCaseStatements.MaxArrowIndent, alignSingleLineCaseStatements_maxArrowIndent)
      .setPreference(DoubleIndentClassDeclaration, doubleIndentClassDeclaration)
      .setPreference(CompactStringConcatenation, compactStringConcatenation)
      .setPreference(FormatXml, formatXml)
      .setPreference(IndentLocalDefs, indentLocalDefs)
      .setPreference(IndentPackageBlocks, indentPackageBlocks)
      .setPreference(IndentSpaces, indentSpaces)
      .setPreference(IndentWithTabs, indentWithTabs)
      .setPreference(MultilineScaladocCommentsStartOnFirstLine, multilineScaladocCommentsStartOnFirstLine)
      .setPreference(PreserveDanglingCloseParenthesis, preserveDanglingCloseParenthesis)
      .setPreference(PreserveSpaceBeforeArguments, preserveSpaceBeforeArguments)
      .setPreference(RewriteArrowSymbols, rewriteArrowSymbols)
      .setPreference(SpaceBeforeColon, spaceBeforeColon)
      .setPreference(SpacesInsideParentheses, spacesInsideParentheses)
      .setPreference(SpacesInsideBrackets, spacesInsideBrackets)
      .setPreference(SpacesWithinPatternBinders, spacesWithinPatternBinders)

    val files = findScalaFiles(path)

    var count = 0

    files.foreach { file ⇒
      try {
        val original = Source.fromFile(file).mkString
        val formatted = ScalaFormatter.format(original, preferences)
        if (original != formatted) {
          writeText(file, formatted)
          count += 1
        }
      } catch {
        case ex: ScalaParserException ⇒
          log.error("Error parsing Scala " + file + ": " + ex.getMessage)
        case ex: Exception ⇒
          log.error("Error formatting " + file + ": " + ex)
      }
    }
    log.info("Modified " + count + " of " + files.size + " .scala files")

  }

}
