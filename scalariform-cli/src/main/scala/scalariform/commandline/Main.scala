package scalariform.commandline

import java.io.IOException
import java.util.Properties
import scalariform.formatter.preferences._
import java.io.File
import scala.io.Source
import scalariform.formatter.ScalaFormatter
import scalariform.parser.ScalaParserException
import java.nio.charset._
import scalariform.utils.Utils._
import scalariform.ScalaVersions

object Main {

  def main(args: Array[String]) {
    exit(process(args))
  }

  def process(args: Array[String]): Int = {

    val parser = new CommandLineOptionParser
    val arguments = args.toList map parser.getArgument

    if (arguments contains Help) {
      printUsage()
      return 0
    }

    if (arguments contains Version) {
      println("Scalariform " + scalariform.VERSION + " for Scala " + ScalaVersions.DEFAULT_VERSION)
      return 0
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

    var preferences: IFormattingPreferences = FormattingPreferences()

    arguments.collect {
      case PreferenceFile(path) ⇒
        try
          preferences = PreferencesImporterExporter.loadPreferences(path)
        catch {
          case e: IOException ⇒
            errors ::= "Error opening " + path + ": " + e.getMessage
        }
    }

    val preferenceOptions = (for (p @ PreferenceOption(_, _) ← arguments) yield p)

    for (PreferenceOption(key, _) ← preferenceOptions if !(AllPreferences.preferencesByKey contains key)) {
      errors ::= "Unrecognised preference: " + key
      showUsage = true
    }

    if (errors.isEmpty) {

      preferences = preferenceOptions.foldLeft(preferences) {
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
    }

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
    val forceOutput = arguments contains ForceOutput
    val inPlace = arguments contains InPlace
    val verbose = arguments contains Verbose

    if (inPlace && test)
      errors ::= "Incompatible arguments --test and --inPlace"

    if (forceOutput && test)
      errors ::= "Incompatible arguments --forceOutput and --inPlace"

    if (forceOutput && files.size > 1)
      errors ::= "Cannot use --forceOutput with multiple files"

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
      return 1
    }

    def log(s: String) = if (verbose) println(s)

    val scalaVersion = arguments.collect {
      case ScalaVersion(scalaVersion) ⇒ scalaVersion
    }.headOption.getOrElse(ScalaVersions.DEFAULT_VERSION)

    log("Assuming source is Scala " + scalaVersion)

    val preferencesText = preferences.preferencesMap.mkString(", ")
    if (preferencesText == "")
      log("Formatting with default preferences.")
    else
      log("Formatting with preferences: " + preferencesText)

    val doFormat: String ⇒ Option[String] = s ⇒
      try
        Some(ScalaFormatter.format(s, preferences, scalaVersion = scalaVersion))
      catch {
        case e: ScalaParserException ⇒ None
      }

    if (test)
      if (files.isEmpty)
        checkSysIn(encoding, doFormat)
      else
        checkFiles(files, encoding, doFormat, log)
    else {
      if (files.isEmpty)
        transformSysInToSysOut(encoding, forceOutput, doFormat)
      else
        files match {
          case List(file) if !inPlace ⇒
            transformFileToSysOut(file, encoding, forceOutput, doFormat)
          case _ ⇒
            transformFilesInPlace(files, encoding, doFormat, log)
        }
    }
  }

  private def checkSource(source: Source, doFormat: String ⇒ Option[String]): FormatResult = {
    val original = source.mkString
    doFormat(original) match {
      case Some(`original`) ⇒ FormattedCorrectly
      case Some(_)          ⇒ NotFormattedCorrectly
      case None             ⇒ DidNotParse
    }
  }

  private def checkSysIn(encoding: String, doFormat: String ⇒ Option[String]): Int = {
    val source = Source.fromInputStream(System.in, encoding)
    if (checkSource(source, doFormat) == FormattedCorrectly) 0 else 1
  }

  private def transformSysInToSysOut(encoding: String, forceOutput: Boolean, doFormat: String ⇒ Option[String]): Int = {
    val original = Source.fromInputStream(System.in, encoding).mkString
    doFormat(original) match {
      case Some(formatted) ⇒
        print(formatted)
        0
      case None ⇒
        if (forceOutput) {
          print(original)
          0
        } else {
          System.err.println("Could not parse text as Scala.")
          1
        }
    }
  }

  private def transformFileToSysOut(file: File, encoding: String, forceOutput: Boolean, doFormat: String ⇒ Option[String]): Int = {
    val original = Source.fromFile(file, encoding).mkString
    doFormat(original) match {
      case Some(formatted) ⇒
        print(formatted)
        0
      case None ⇒
        if (forceOutput) {
          print(original)
          0
        } else {
          System.err.println("Could not parse " + file.getPath + " as Scala")
          1
        }

    }
  }

  private def transformFilesInPlace(files: Seq[File], encoding: String, doFormat: String ⇒ Option[String], log: String ⇒ Unit): Int = {
    var problems = false
    for (file ← files) {
      val original = Source.fromFile(file, encoding).mkString
      doFormat(original) match {
        case Some(formatted) ⇒
          if (formatted == original)
            log(".              " + file)
          else {
            log("[Reformatted]  " + file)
            writeText(file, formatted, Some(encoding))
          }
        case None ⇒
          log("[Parse error]   " + file.getPath)
          problems = true
      }
    }
    if (problems) 1 else 0
  }

  private def checkFiles(files: Seq[File], encoding: String, doFormat: String ⇒ Option[String], log: String ⇒ Unit): Int = {
    val allCorrect = files.forall { file ⇒
      val source = Source.fromFile(file, encoding)
      val formatResult = checkSource(source, doFormat)
      val resultString = formatResult match {
        case FormattedCorrectly    ⇒ "OK"
        case NotFormattedCorrectly ⇒ "FAILED"
        case DidNotParse           ⇒ "ERROR"
      }
      val padding = " " * (6 - resultString.length)
      log("[" + resultString + "]" + padding + " " + file)
      formatResult == FormattedCorrectly
    }
    if (allCorrect) 0 else 1
  }

  private sealed trait FormatResult
  private case object FormattedCorrectly extends FormatResult
  private case object NotFormattedCorrectly extends FormatResult
  private case object DidNotParse extends FormatResult

  private def printUsage() {
    println("Usage: scalariform [options] [files...]")
    println()
    println("Options:")
    println("  --encoding=<encoding>                Set the encoding, e.g. UTF-8. If not set, defaults to the platform default encoding.")
    println("  --fileList=<path>, -l=<path>         Read the list of input file(s) from a text file (one per line)")
    println("  --forceOutput, -f                    Return the input unchanged if the file cannot be parsed correctly. (Only works for input on stdin or single files)")
    println("  --help, -h                           Show help")
    println("  --inPlace, -i                        Replace the input file(s) in place with a formatted version.")
    println("  --preferenceFile=<path>, -p=<path>   Read preferences from a properties file")
    println("  --scalaVersion=<v>, -s=<v>           Assume the source is written against the given version of Scala (e.g. 2.9.2). Default is runtime version.")
    println("  --test, -t                           Check the input(s) to see if they are correctly formatted, return a non-zero error code if not.")
    println("  --verbose, -v                        Verbose output")
    println("  --version                            Show Scalariform version")
    println()
    println("Preferences:")
    val descriptionColumn = 61
    val sortedPreferences = AllPreferences.preferencesByKey.keySet.toList.sorted

    for {
      key ← sortedPreferences
      preference = AllPreferences.preferencesByKey(key)
      if preference.preferenceType == BooleanPreference
    } {
      val optionText = "  [+|-]" + key
      val filler = " " * (descriptionColumn - optionText.length)
      println(optionText + filler + "Enable/disable " + preference.description)
    }

    for {
      key ← sortedPreferences
      preference = AllPreferences.preferencesByKey(key)
      IntegerPreference(min, max) ← Some(preference.preferenceType)
    } {
      val optionText = "  -" + key + "=[" + min + "-" + max + "]"
      val filler = " " * (descriptionColumn - optionText.length)
      println(optionText + filler + "Set " + preference.description)
    }

    println()
    println("Examples:")
    println(" scalariform +spaceBeforeColon -alignParameters -indentSpaces=2 --inPlace foo.scala")
    println(" find . -name '*.scala' | xargs scalariform +rewriteArrowSymbols --verbose --test")
    println(" echo 'class A ( n  :Int )' | scalariform")
  }

}
