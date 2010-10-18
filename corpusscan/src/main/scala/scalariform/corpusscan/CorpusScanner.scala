package scalariform.corpusscan

import scalariform.lexer.{ Token ⇒ _, _ }
import scalariform.formatter._
import scalariform.formatter.preferences._
import scalariform.parser._
import scala.util.parsing.input._
import scala.util.parsing.combinator._
import java.io.File
import scala.io.Source
import scalariform.utils.Utils.writeText

sealed trait ParseFault
case object TokensDoNotCoverSource extends ParseFault
case object UnsuccessfulParse extends ParseFault
case object BadAstTokens extends ParseFault
case class ParseException(e: Throwable) extends ParseFault

object CorpusScanner extends SpecificFormatter {

  def attemptToParse(file: File): Option[ParseFault] = {
    val source = getText(file)
    val sourceAgain = ScalaLexer.rawTokenise(source).map(_.getText).mkString
    if (source != sourceAgain)
      return Some(TokensDoNotCoverSource)
    val (lexer, tokens) = ScalaLexer.tokeniseFull(file)
    try {
      val result = new ScalaParser(tokens.toArray).compilationUnitOrScript()
      if (result.tokens != tokens.init) /* drop EOF */
        Some(BadAstTokens)
      else
        None
    } catch {
      case e: ScalaParserException ⇒ Some(UnsuccessfulParse)
    }
  }

  private def getText(file: File) = Source.fromFile(file).mkString

  def formatFile(file: File) {
    val source = getText(file)
    val formatted = format(source)(FormattingPreferences())
    val formatted2 = format(formatted)(FormattingPreferences())
    require(formatted == formatted2, "Idempotency failure")
    writeText(file, formatted)
    //    for (parseFault <- attemptToParse(file))
    //      throw new RuntimeException(parseFault.toString)
  }

  type Result = CompilationUnit

  def parse(parser: ScalaParser) = parser.compilationUnitOrScript()

  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState(indentLevel = 0))

}

object Runner {
  def checkParser() {
    val files = ScalaFileWalker.findScalaFiles("/home/matt/corpus")
    var count = 0
    var parsedCount = 0
    for (file ← files) {
      val parsed = CorpusScanner.attemptToParse(file).isEmpty
      if (parsed)
        parsedCount += 1
      if (!parsed)
        println((if (parsed) "OK   " else "FAIL!") + " -- " + file)
      count += 1
    }
    println("Total scanned: " + count)
    println("Number parsed successfully: " + parsedCount)
    println("Parse failures: " + (count - parsedCount))
  }

  def formatInPlace() {
    var count = 0
    for (file ← ScalaFileWalker.findScalaFiles("/home/matt/corpus-to-modify-2")) {
      print("Formatting: " + file)
      CorpusScanner.formatFile(file)
      val parsed = CorpusScanner.attemptToParse(file)
      require(parsed == None, parsed.toString)
      println()
      count += 1
    }
    println(count + " files formatted.")
  }

  def main(args: Array[String]) = formatInPlace()

}
