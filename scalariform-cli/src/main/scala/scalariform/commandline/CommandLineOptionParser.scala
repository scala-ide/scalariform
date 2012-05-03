package scalariform.commandline

import scala.util.parsing.input._
import scala.util.parsing.combinator._

class CommandLineOptionParser extends RegexParsers {

  lazy val option: Parser[CommandLineArgument] =
    phrase(help) | phrase(version) | phrase(scalaVersion) | phrase(test) | phrase(forceOutput) | phrase(inPlace) | phrase(verbose) | phrase(fileList) |
      phrase(encoding) | phrase(toggle) | phrase(preferenceFile) | phrase(preferenceOption) | phrase(badOption)

  lazy val test = ("--test" | "-t") ^^^ Test

  lazy val forceOutput = ("--forceOutput" | "-f") ^^^ ForceOutput

  lazy val inPlace = ("--inPlace" | "-i") ^^^ InPlace

  lazy val verbose = ("--verbose" | "-v") ^^^ Verbose

  lazy val help = ("--help" | "-help" | "-h") ^^^ Help

  lazy val version = ("--version" | "-version") ^^^ Version

  lazy val scalaVersion = ("--scalaVersion=" | "-s=") ~> """(\d|\.)+""".r ^^ ScalaVersion

  lazy val fileList = ("--fileList=" | "-l=") ~ ".+".r ^^ { case (_ ~ name) ⇒ FileList(name) }

  lazy val encoding = "--encoding=" ~ ".+".r ^^ { case (_ ~ encoding) ⇒ Encoding(encoding) }

  lazy val toggle = plusOrMinus ~ preferenceKey ^^ { case onOrOff ~ key ⇒ PreferenceOption(key, onOrOff.toString) }

  lazy val plusOrMinus = "+" ^^^ true | "-" ^^^ false

  lazy val preferenceFile = ("--preferenceFile=" | "-p=") ~ ".+".r ^^ { case (_ ~ name) ⇒ PreferenceFile(name) }

  lazy val preferenceOption = "-" ~ preferenceKey ~ "=" ~ """(\w|\.)+""".r ^^ { case (_ ~ key ~ _ ~ value) ⇒ PreferenceOption(key, value) }

  lazy val preferenceKey: Parser[String] = """[a-zA-Z.]+""".r

  lazy val badOption = guard(plusOrMinus) ~> ".*".r ^^ { BadOption(_) }

  def getArgument(s: String) = parse(option, s) getOrElse FileName(s)
}

sealed trait CommandLineArgument

case class PreferenceOption(preferenceKey: String, value: String) extends CommandLineArgument
case class PreferenceFile(name: String) extends CommandLineArgument
case class FileName(name: String) extends CommandLineArgument
case class FileList(name: String) extends CommandLineArgument
case class Encoding(encoding: String) extends CommandLineArgument
case object Test extends CommandLineArgument
case object ForceOutput extends CommandLineArgument
case object InPlace extends CommandLineArgument
case object Verbose extends CommandLineArgument
case object Help extends CommandLineArgument
case object Version extends CommandLineArgument
case class ScalaVersion(scalaVersion: String) extends CommandLineArgument
case class BadOption(name: String) extends CommandLineArgument
