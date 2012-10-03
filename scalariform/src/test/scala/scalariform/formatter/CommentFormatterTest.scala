package scalariform.formatter

import scalariform.parser._
import scalariform.formatter._
import scalariform.formatter.preferences._

// format: OFF
class CommentFormatterTest extends AbstractFormatterTest {

  type Result = CompilationUnit

  def parse(parser: ScalaParser) = parser.scriptBody()
  
  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState())

  override val debug = false

  """/** 
    |*a
    |b
    | */c""" ==>
  """/**
    | * a
    | * b
    | */
    |c"""

  """/** 
    |*a
    |b
    | */""" ==>
  """/**
    | * a
    | * b
    | */
    |"""

  """/**
    | *
    | *Wibble*/ 
    |class X""" ==>
  """/**
    | *
    | * Wibble
    | */
    |class X"""

  """/***/
    |class A""" ==>
  """/***/
    |class A"""

  """/** */
    |class A""" ==>
  """/** */
    |class A"""

  """/** a */
    |class A""" ==>
  """/** a */
    |class A"""

  """/**
    | * {{
    | *   wibble
    | * }}
    | */
    |class A""" ==>
  """/**
    | * {{
    | *   wibble
    | * }}
    | */
    |class A"""

  """/**
    |*
    |*/""" ==>
  """/**
    | *
    | */
    |"""

  """/** a
    |  * b */""" ==>
  """/**
    | * a
    | * b
    | */
    |"""
      
  // nested comments
  """/**
    |/*
    |*/
    |*/""" ==>
  """/**
    | * /*
    | * */
    | */
    |"""
      
  {
  implicit val formattingPreferences = FormattingPreferences.setPreference(MultilineScaladocCommentsStartOnFirstLine, true)

  """/** This method applies f to each 
    | *  element of the given list.
    | */""" ==>
  """/** This method applies f to each
    | *  element of the given list.
    | */
    |""" 

  """/** Foo
    |Bar
    |*Baz  */""" ==>
  """/** Foo
    | *  Bar
    | *  Baz
    | */
    |"""

  """/** Foo
    |*/""" ==>
  """/** Foo
    | */
    |"""

  """/**
    |*/""" ==>
  """/**
    | */
    |"""
  }
  
  {
  implicit val formattingPreferences = FormattingPreferences.setPreference(PlaceScaladocAsterisksBeneathSecondAsterisk, true)

  """/** This method applies f to each 
    | * element of the given list.
    | */""" ==>
  """/**
    |  * This method applies f to each
    |  * element of the given list.
    |  */
    |""" 

  """/** Foo
    |Bar
    |*Baz  */""" ==>
  """/**
    |  * Foo
    |  * Bar
    |  * Baz
    |  */
    |"""

  """/** Foo
    |*/""" ==>
  """/**
    |  * Foo
    |  */
    |"""

  """/**
    |*/""" ==>
  """/**
    |  */
    |"""
  }
  
  {
  implicit val formattingPreferences = FormattingPreferences
    .setPreference(MultilineScaladocCommentsStartOnFirstLine, true)
    .setPreference(PlaceScaladocAsterisksBeneathSecondAsterisk, true)
  """/** This method applies f to each 
    | * element of the given list.
    | */""" ==>
  """/** This method applies f to each
    |  * element of the given list.
    |  */
    |""" 

  """/** Foo
    |Bar
    |*Baz  */""" ==>
  """/** Foo
    |  * Bar
    |  * Baz
    |  */
    |"""

  """/** Foo
    |*/""" ==>
  """/** Foo
    |  */
    |"""

  """/**
    |*/""" ==>
  """/**
    |  */
    |"""
  }

  {
    """object Main {
      |/***
      |scalaVersion := "ololo"
      |
      |libraryDependencies ++= Seq(
      |  "net.databinder" % "dispatch-twitter_2.9.0-1" % "0.8.3",
      |  "net.databinder" % "dispatch-http_2.9.0-1" % "0.8.3"
      |)
      |*/
      |}""" ==>
      """object Main {
        |/***
        |scalaVersion := "ololo"
        |
        |libraryDependencies ++= Seq(
        |  "net.databinder" % "dispatch-twitter_2.9.0-1" % "0.8.3",
        |  "net.databinder" % "dispatch-http_2.9.0-1" % "0.8.3"
        |)
        |*/
        |}"""
  }
}
