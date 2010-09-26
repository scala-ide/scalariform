package scalariform.commandline

import scalariform.formatter.preferences._
import java.io.File
import scala.io.Source
import scalariform.formatter.ScalaFormatter
import scalariform.parser.ScalaParserException
import scalariform.utils.Utils.writeText
import java.nio.charset._

object Main {

  def main(args: Array[String]) {

    val parser = new CommandLineOptionParser
    val arguments = args.toList map parser.getArgument

    if (arguments contains Help) {
      printUsage()
      exit(0)
    }

    if (arguments contains Version) {
      println("Scalariform " + scalariform.VERSION)
      exit(0)
    }

    var errors: List[String] = Nil
    var showUsage = false

    for (BadOption(s) ← arguments) {
      errors ::= "Unrecognised option: " + s
      showUsage = true
    }

    val encoding: String = arguments.collect {
      case Encoding(encoding) ⇒ encoding
    }.headOption.getOrElse(System getProperty "file.encoding")

    try {
      Charset.forName(encoding)
    } catch {
      case e: UnsupportedCharsetException ⇒
        errors ::= "Unsupported encoding " + encoding
      case e: IllegalCharsetNameException ⇒
        errors ::= "Illegal encoding " + encoding
    }

    val preferenceOptions = (for (p@PreferenceOption(_, _) ← arguments) yield p)

    for (PreferenceOption(key, _) ← preferenceOptions if !(AllPreferences.preferencesByKey contains key)) {
      errors ::= "Unrecognised preference: " + key
      showUsage = true
    }

    val preferences = if (errors.isEmpty)
      preferenceOptions.foldLeft(FormattingPreferences()) {
        case (preferences, PreferenceOption(key, valueString)) ⇒
          val descriptor = AllPreferences.preferencesByKey(key)
          def processDescriptor[T](descriptor: PreferenceDescriptor[T]) = {
            descriptor.preferenceType.parseValue(valueString) match {
              case Right(value) ⇒ preferences.setPreference(descriptor, value)
              case Left(error) ⇒
                errors ::= "Could not parse value for preference " + key + ", " + error
                preferences
            }
          }
          processDescriptor(descriptor)
      }
    else
      FormattingPreferences()

    def getFiles(): List[File] = {
      var files: List[File] = Nil
      def addFile(fileName: String) {
        val file = new File(fileName)
        if (!file.exists)
          errors ::= "No such file " + file
        if (file.isDirectory)
          errors ::= "Cannot format a directory (" + file + ")"
        files ::= file
      }
      for (FileList(listName) ← arguments) {
        val listFile = new File(listName)
        if (!listFile.exists)
          errors ::= "No such file: file list " + listFile
        else if (listFile.isDirectory)
          errors ::= "Path is a directory: file list " + listFile
        else
          Source.fromFile(listFile, encoding).getLines foreach addFile
      }
      for (FileName(fileName) ← arguments) addFile(fileName)
      files.reverse
    }

    val files = getFiles()

    val test = arguments contains Test
    val inPlace = arguments contains InPlace
    val verbose = arguments contains Verbose

    if (inPlace && test)
      errors ::= "Incompatible arguments --test and --inPlace"

    if (inPlace && files.isEmpty)
      errors ::= "Need to provide at least one file to modify in place"

    if (!inPlace && !test && files.size > 1)
      errors ::= "Cannot have more than one input file unless using --test or --inPlace"

    if (verbose && !inPlace && !test)
      errors ::= "Will not be verbose unless using --test or --inPlace"

    if (!errors.isEmpty) {
      errors.reverse foreach System.err.println
      if (showUsage)
        printUsage()
      exit(1)
    }

    def log(s: String) = if (verbose) println(s)

    val preferencesText = preferences.preferencesMap.mkString(", ")
    if (preferencesText.isEmpty)
      log("Formatting with default preferences.")
    else
      log("Formatting with preferences: " + preferencesText)

    if (test) {
      trait FormatResult
      case object FormattedCorrectly extends FormatResult
      case object NotFormattedCorrectly extends FormatResult
      case object DidNotParse extends FormatResult

      var allFormattedCorrectly = true
      def checkSource(source: Source): FormatResult = {
        val original = source.mkString
        try {
          val formatted = ScalaFormatter.format(original, preferences)
          // TODO: Sometimes get edits which cancel each other out
          // val edits = ScalaFormatter.formatAsEdits(source.mkString, preferences)
          // edits.isEmpty
          if (formatted == original) FormattedCorrectly else NotFormattedCorrectly
        } catch {
          case e: ScalaParserException ⇒ DidNotParse
        }
      }
      if (files.isEmpty) {
        val formatResult = checkSource(Source.fromInputStream(System.in, encoding))
        allFormattedCorrectly &= (formatResult == FormattedCorrectly)
      } else
        for (file ← files) {
          val formatResult = checkSource(Source.fromFile(file, encoding))
          val resultString = formatResult match {
            case FormattedCorrectly ⇒ "OK"
            case NotFormattedCorrectly ⇒ "FAILED"
            case DidNotParse ⇒ "ERROR"
          }
          val padding = " " * (6 - resultString.length)
          log("[" + resultString + "]" + padding + " " + file)
          allFormattedCorrectly &= (formatResult == FormattedCorrectly)
        }
      exit(if (allFormattedCorrectly) 0 else 1)
    } else {
      if (files.isEmpty) {
        val original = Source.fromInputStream(System.in, encoding).mkString
        try {
          val formatted = ScalaFormatter.format(original, preferences)
          print(formatted)
        } catch {
          case e: ScalaParserException ⇒
            System.err.println("Could not parse text as Scala.")
            exit(1)
        }
      } else {
        var problems = false
        for (file ← files) {
          val original = Source.fromFile(file, encoding).mkString
          val formattedOption = try {
            Some(ScalaFormatter.format(original, preferences))
          } catch {
            case e: ScalaParserException ⇒
              log("[Parse error]   " + file.getPath)
              problems = true
              None
          }
          for (formatted ← formattedOption) {
            if (inPlace)
              if (formatted == original)
                log(".              " + file)
              else {
                log("[Reformatted]  " + file)
                writeText(file, formatted, Some(encoding))
              }
            else
              print(formatted)
          }
        }
        if (problems)
          exit(1)
      }
    }
  }

  private def printUsage() {
    println("Usage: scalariform [options] [files...]")
    println()
    println("Options:")
    println("  --encoding=<encoding>           Set the encoding, e.g. UTF-8. If not set, defaults to the platform default encoding.")
    println("  --fileList=<path>, -l=<path>    Read the list of input file(s) from a text file (one per line)")
    println("  --help, -h                      Show help")
    println("  --inPlace, -i                   Replace the input file(s) in place with a formatted version.")
    println("  --test, -t                      Check the input(s) to see if they are correctly formatted, return a non-zero error code if not.")
    println("  --verbose, -v                   Verbose output")
    println("  --version                       Show Scalariform version")
    println()
    println("Preferences:")
    for (key ← AllPreferences.preferencesByKey.keySet.toList.sorted) {
      def handlePreference[T](preference: PreferenceDescriptor[T]) {
        preference.preferenceType match {
          case BooleanPreference ⇒
            val optionText = "  [+|-]" + key
            val filler = " " * (38 - optionText.length)
            println(optionText + filler + "Enable/disable " + preference.description)
          case IntegerPreference(min, max) ⇒
            val optionText = "  -" + key + "=[" + min + "-" + max + "]"
            val filler = " " * (38 - optionText.length)
            println(optionText + filler + "Set " + preference.description)
        }
      }
      handlePreference(AllPreferences.preferencesByKey(key))
    }
    println()
    println("Examples:")
    println(" scalariform +spaceBeforeColon -alignParameters -indentSpaces=2 --inPlace foo.scala")
    println(" find . -name '*.scala' | xargs scalariform +rewriteArrowSymbols --verbose --test")
    println(" echo 'class A ( n  :Int )' | scalariform")
  }

}
