package scalariform.commandline

import scala.util.parsing.combinator._

class CommandLineOptionParser extends RegexParsers {
  type Arg = Parser[CommandLineArgument]

  lazy val option: Arg =
    phrase(help) | phrase(version) | phrase(scalaVersion) | phrase(stdin) | phrase(stdout) | phrase(recurse) |
      phrase(test) | phrase(forceOutput) | phrase(quiet) | phrase(fileList) | phrase(encoding) | phrase(toggle) |
      phrase(preferenceFile) | phrase(preferenceOption) | phrase(badOption)

  lazy val test: Parser[Test.type] = ("--test" | "-t") ^^^ Test

  lazy val forceOutput: Arg = ("--forceOutput" | "-f") ^^^ ForceOutput

  lazy val stdout: Arg = "--stdout" ^^^ Stdout

  lazy val stdin: Arg = "--stdin" ^^^ Stdin

  lazy val quiet: Arg = ("--quiet" | "-q") ^^^ Quiet

  lazy val recurse: Arg = ("--recurse" | "-r") ^^^ Recurse

  lazy val help: Arg = ("--help" | "-help" | "-h") ^^^ Help

  lazy val version: Arg = ("--version" | "-version") ^^^ Version

  lazy val scalaVersion: Arg = ("--scalaVersion=" | "-s=") ~> """(\d|\.)+""".r ^^ ScalaVersion

  lazy val fileList: Arg = ("--fileList=" | "-l=") ~> ".+".r ^^ FileList

  lazy val encoding: Arg = "--encoding=" ~> ".+".r ^^ Encoding

  lazy val toggle: Arg = plusOrMinus ~ preferenceKey ^^ { case onOrOff ~ key ⇒ PreferenceOption(key, onOrOff.toString) }

  lazy val plusOrMinus: Parser[Boolean] = "+" ^^^ true | "-" ^^^ false

  lazy val preferenceFile: Arg = ("--preferenceFile=" | "-p=") ~> ".+".r ^^ PreferenceFile

  lazy val preferenceOption: Arg = ("-" ~> preferenceKey <~ "=") ~ """(\w|\.)+""".r ^^ {
    case (key ~ value) ⇒ PreferenceOption(key, value)
  }

  lazy val preferenceKey: Parser[String] = """[a-zA-Z.]+""".r

  lazy val badOption: Arg = guard(plusOrMinus) ~> ".*".r ^^ BadOption

  def getArgument(s: String): CommandLineArgument = parse(option, s) getOrElse FileName(s)
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
