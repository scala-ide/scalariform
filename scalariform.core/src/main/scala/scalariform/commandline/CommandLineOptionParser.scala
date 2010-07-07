package scalariform.commandline

import scala.util.parsing.input._
import scala.util.parsing.combinator._

class CommandLineOptionParser extends JavaTokenParsers with RegexParsers {

  lazy val option: Parser[CommandLineArgument] =
    phrase(help) | phrase(test) | phrase(inPlace) | phrase(verbose) | phrase(fileList) |
      phrase(toggle) | phrase(preferenceOption) | phrase(badOption)

  lazy val test = ("--test" | "-t") ^^^ Test

  lazy val inPlace = ("--inPlace" | "-i") ^^^ InPlace

  lazy val verbose = ("--verbose" | "-v") ^^^ Verbose

  lazy val help = ("--help" | "-help" | "-h") ^^^ Help

  lazy val fileList = ("--fileList=" | "-l=") ~ ".+".r ^^ { case (_ ~ name) ⇒ FileList(name) }

  lazy val toggle = plusOrMinus ~ ident ^^ { case onOrOff ~ key ⇒ PreferenceOption(key, onOrOff.toString) }

  lazy val plusOrMinus = "+" ^^^ true | "-" ^^^ false

  lazy val preferenceOption = "-" ~ ident ~ "=" ~ "\\w+".r ^^ { case (_ ~ key ~ _ ~ value) ⇒ PreferenceOption(key, value) }

  lazy val badOption = guard(plusOrMinus) ~> ".*".r ^^ { BadOption(_) }

  def getArgument(s: String) = parse(option, s) getOrElse FileName(s)
}

sealed trait CommandLineArgument

case class PreferenceOption(preferenceKey: String, value: String) extends CommandLineArgument
case class FileName(name: String) extends CommandLineArgument
case class FileList(name: String) extends CommandLineArgument
case object Test extends CommandLineArgument
case object InPlace extends CommandLineArgument
case object Verbose extends CommandLineArgument
case object Help extends CommandLineArgument
case class BadOption(name: String) extends CommandLineArgument
