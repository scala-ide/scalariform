package scalariform.commandline

import java.io.File
import java.io.IOException
import java.nio.charset._
import scala.io.Source
import scalariform.formatter.preferences._
import scalariform.formatter.ScalaFormatter
import scalariform.parser.ScalaParserException
import scalariform.utils.Utils._
import scalariform.ScalaVersions

object Main {

  def main(args: Array[String]) {
    sys.exit(if (process(args)) 1 else 0)
  }

  def process(args: Array[String]): Boolean = {

    val parser = new CommandLineOptionParser
    val arguments = args.toList map parser.getArgument

    if (arguments contains Help) {
      printUsage()
      return false
    }

    if (arguments contains Version) {
      println("Scalariform " + scalariform.BuildInfo.version + " (runtime Scala " + ScalaVersions.DEFAULT_VERSION + ")")
      return false
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

    try
      Charset.forName(encoding)
    catch {
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

    val recurse = arguments contains Recurse

    def getFiles(): List[File] = {
      var files: List[File] = Nil
      def addFile(fileName: String) {
        val file = new File(fileName)
        if (!file.exists)
          errors ::= "No such file " + file
        if (file.isDirectory)
          if (recurse)
            files :::= ScalaFileWalker.findScalaFiles(file)
          else
            errors ::= "Cannot format a directory (" + file + ")"
        else
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
    val quiet = arguments contains Quiet
    val stdout = arguments contains Stdout
    val stdin = arguments contains Stdin

    if (files.nonEmpty && stdin)
      errors ::= "Cannot specify files when using --stdin"

    if (files.isEmpty && !stdin)
      errors ::= "Must specify a file or use --stdin (run with --help for full options)"

    if (forceOutput && !stdout && !stdin)
      errors ::= "--forceOutput can only be used with --stdout or --stdin"

    if (forceOutput && files.size > 1)
      errors ::= "Cannot use --forceOutput with multiple files"

    if (!errors.isEmpty) {
      for (error ← errors.reverse)
        System.err.println("Error: " + error)
      if (showUsage)
        printUsage()
      return true
    }

    def log(s: String) = if (!quiet && !stdout && !stdin) println(s)

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
      if (stdin)
        !checkSysIn(encoding, doFormat)
      else
        !checkFiles(files, encoding, doFormat, log)
    else {
      if (stdin)
        transformSysInToSysOut(encoding, forceOutput, doFormat)
      else
        files match {
          case List(file) if stdout ⇒
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

  /**
   * @return true iff the source from stdin is formatted correctly
   */
  private def checkSysIn(encoding: String, doFormat: String ⇒ Option[String]): Boolean = {
    val source = Source.fromInputStream(System.in, encoding)
    checkSource(source, doFormat) == FormattedCorrectly
  }

  private def transformSysInToSysOut(encoding: String, forceOutput: Boolean, doFormat: String ⇒ Option[String]): Boolean = {
    val original = Source.fromInputStream(System.in, encoding).mkString
    doFormat(original) match {
      case Some(formatted) ⇒
        print(formatted)
        false
      case None ⇒
        if (forceOutput) {
          print(original)
          false
        } else {
          System.err.println("Error: Could not parse text as Scala.")
          true
        }
    }
  }

  private def transformFileToSysOut(file: File, encoding: String, forceOutput: Boolean, doFormat: String ⇒ Option[String]): Boolean = {
    val original = Source.fromFile(file, encoding).mkString
    doFormat(original) match {
      case Some(formatted) ⇒
        print(formatted)
        false
      case None ⇒
        if (forceOutput) {
          print(original)
          false
        } else {
          System.err.println("Error: Could not parse " + file.getPath + " as Scala")
          true
        }
    }
  }

  private def transformFileInPlace(file: File, encoding: String, doFormat: String ⇒ Option[String], log: String ⇒ Unit): Boolean = {
    val original = Source.fromFile(file, encoding).mkString
    doFormat(original) match {
      case Some(formatted) ⇒
        if (formatted == original)
          log(".              " + file)
        else {
          log("[Reformatted]  " + file)
          writeText(file, formatted, Some(encoding))
        }
        false
      case None ⇒
        log("[Parse error]   " + file.getPath)
        true
    }
  }

  private def transformFilesInPlace(files: Seq[File], encoding: String, doFormat: String ⇒ Option[String], log: String ⇒ Unit): Boolean = {
    var problems = false
    for (file ← files)
      problems &= transformFileInPlace(file, encoding, doFormat, log)
    problems
  }

  /**
   * @return true iff file is already formatted correctly
   */
  private def checkFile(file: File, encoding: String, doFormat: String ⇒ Option[String], log: String ⇒ Unit): Boolean = {
    val source = Source.fromFile(file, encoding)
    val formatResult = checkSource(source, doFormat)
    val resultString = formatResult match {
      case FormattedCorrectly    ⇒ "OK"
      case NotFormattedCorrectly ⇒ "FAILED"
      case DidNotParse           ⇒ "ERROR"
    }
    val padding = " " * (6 - resultString.length)
    log("[" + resultString + "]" + padding + " " + file)
    formatResult != FormattedCorrectly
  }

  /**
   * @return true iff all files are already formatted correctly
   */
  private def checkFiles(files: Seq[File], encoding: String, doFormat: String ⇒ Option[String], log: String ⇒ Unit): Boolean = {
    var allPassed = true
    for (file ← files)
      allPassed &= checkFile(file, encoding, doFormat, log)
    allPassed
  }

  private sealed trait FormatResult
  private case object FormattedCorrectly extends FormatResult
  private case object NotFormattedCorrectly extends FormatResult
  private case object DidNotParse extends FormatResult

  private def printUsage() {
    println("Usage: scalariform [options] [files...]")
    println()
    println("Options:")
    println("  --encoding=<encoding>                Set the encoding, e.g. UTF-8. If not set, defaults to the platform default encoding (currently " + System.getProperty("file.encoding") + ").")
    println("  --fileList=<path>, -l=<path>         Read the list of input file(s) from a text file (one per line)")
    println("  --forceOutput, -f                    If using --stdout, print the source unchanged if it cannot be parsed correctly.")
    println("  --help, -h                           Show help")
    println("  --preferenceFile=<path>, -p=<path>   Read preferences from a properties file")
    println("  --quiet, -q                          Work quietly")
    println("  --recurse, -r                        If any given file is a directory, recurse beneath it and collect all .scala files for processing")
    println("  --scalaVersion=<v>, -s=<v>           Assume the source is written against the given version of Scala (e.g. 2.9.2). Default is runtime version (currently " + ScalaVersions.DEFAULT_VERSION + ").")
    println("  --stdin                              Read Scala source from standard input")
    println("  --stdout                             Write the formatted output to standard output")
    println("  --test, -t                           Check the input(s) to see if they are correctly formatted, return a non-zero error code if not.")
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
    println(" scalariform +spaceBeforeColon -alignParameters -indentSpaces=2 foo.scala")
    println(" scalariform +rewriteArrowSymbols --test --recurse .")
    println(" echo 'class A ( n  :Int )' | scalariform --stdin")
  }

}
