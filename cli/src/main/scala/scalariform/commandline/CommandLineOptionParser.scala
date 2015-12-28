package scalariform.commandline

import scala.util.parsing.input._
import scala.util.parsing.combinator._

class CommandLineOptionParser extends RegexParsers {

  lazy val option: Parser[CommandLineArgument] =
    phrase(help) | phrase(version) | phrase(scalaVersion) | phrase(stdin) | phrase(stdout) | phrase(recurse) |
      phrase(test) | phrase(forceOutput) | phrase(quiet) | phrase(fileList) | phrase(encoding) | phrase(toggle) |
      phrase(preferenceFile) | phrase(preferenceOption) | phrase(badOption)

  lazy val test = ("--test" | "-t") ^^^ Test

  lazy val forceOutput = ("--forceOutput" | "-f") ^^^ ForceOutput

  lazy val stdout = "--stdout" ^^^ Stdout

  lazy val stdin = "--stdin" ^^^ Stdin

  lazy val quiet = ("--quiet" | "-q") ^^^ Quiet

  lazy val recurse = ("--recurse" | "-r") ^^^ Recurse

  lazy val help = ("--help" | "-help" | "-h") ^^^ Help

  lazy val version = ("--version" | "-version") ^^^ Version

  lazy val scalaVersion = ("--scalaVersion=" | "-s=") ~> """(\d|\.)+""".r ^^ ScalaVersion

  lazy val fileList = ("--fileList=" | "-l=") ~> ".+".r ^^ FileList

  lazy val encoding = "--encoding=" ~> ".+".r ^^ Encoding

  lazy val toggle = plusOrMinus ~ preferenceKey ^^ { case onOrOff ~ key ⇒ PreferenceOption(key, onOrOff.toString) }

  lazy val plusOrMinus = "+" ^^^ true | "-" ^^^ false

  lazy val preferenceFile = ("--preferenceFile=" | "-p=") ~> ".+".r ^^ PreferenceFile

  lazy val preferenceOption = ("-" ~> preferenceKey <~ "=") ~ """(\w|\.)+""".r ^^ {
    case (key ~ value) ⇒ PreferenceOption(key, value)
  }

  lazy val preferenceKey: Parser[String] = """[a-zA-Z.]+""".r

  lazy val badOption = guard(plusOrMinus) ~> ".*".r ^^ BadOption

  def getArgument(s: String) = parse(option, s) getOrElse FileName(s)
}

sealed trait CommandLineArgument

case class PreferenceOption(preferenceKey: String, value: String) extends CommandLineArgument
case class PreferenceFile(name: String) extends CommandLineArgument
case class FileName(name: String) extends CommandLineArgument
case class FileList(name: String) extends CommandLineArgument
case class Encoding(encoding: String) extends CommandLineArgument
case object Test extends CommandLineArgument
case object Stdout extends CommandLineArgument
case object Stdin extends CommandLineArgument
case object ForceOutput extends CommandLineArgument
case object Quiet extends CommandLineArgument
case object Help extends CommandLineArgument
case object Version extends CommandLineArgument
case object Recurse extends CommandLineArgument
case class ScalaVersion(scalaVersion: String) extends CommandLineArgument
case class BadOption(name: String) extends CommandLineArgument
