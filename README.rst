Scalariform
===========

Scalariform is a code formatter for Scala 2.8+. It is a library and a
stand-alone command line tool, with integrations available for various
editors (see below).

Currently, Scalariform supports only a limited set of options. The
plan is to add further features as and when people ask for them, so
please do raise a ticket if it doesn't format your code the way you'd
like it, and I'll see what I can do.

Scalariform is licenced under `The MIT Licence`_.

.. _Scala Style Guide: http://davetron5000.github.com/scala-style/
.. _The MIT Licence: http://www.opensource.org/licenses/mit-license.php

Download
--------

Scalariform is available from Scala-tools.org:

  http://scala-tools.org/repo-releases/org/scalariform/scalariform_2.8.0/0.0.9/

If you're using sbt, you can declare a dependency as follows::

  val scalariform = "org.scalariform" %% "scalariform" % "0.0.9"

Integration with Eclipse
------------------------

Scala IDE for Eclipse uses Scalariform for formatting:

  http://download.scala-ide.org/

(See http://www.assembla.com/wiki/show/scala-ide/Requirements_and_Installation 
for more detailed instructions.)

Formatting works the same in the Scala editor; that is, either

- Right click in the editor -> Source -> Format
- Press Ctrl-Shift-F

To configure preferences, go to Window -> Preferences -> Scala -> Formatter

It can also perform formatting as a save action (Window -> Preferences -> Java -> Editor -> Save Actions).

Integration with Emacs/ENSIME
-----------------------------

"`ENSIME`_ uses the Scalariform library to format Scala sources. Type C-c C-v f to format the current buffer." 

  http://aemon.com/file_dump/ensime_manual.html#tth_sEc4.8

.. _ENSIME: http://github.com/aemoncannon/ensime

Integration with jEdit
----------------------

See `ScalaSidekick`_ by Stefan Ettrup:

.. _ScalaSidekick: http://github.com/StefanE/ScalaSidekick

Run Plugins -> scalaSidekickPlugin -> Format Scala File

Integration with Maven
----------------------

There is a Maven plugin to run Scalariform contributed by `Adam
Crain`_ on scala-tools.

.. _Adam Crain: https://github.com/jadamcrain

Usage::

  <plugin>
    <groupId>org.scalariform</groupId>
    <artifactId>scalariform-maven-plugin</artifactId>
    <version>0.0.9</version>
    <executions>
      <execution>
        <phase>process-sources</phase>
        <goals>
          <goal>format</goal>
        </goals>
        <configuration>
          <rewriteArrowSymbols>true</rewriteArrowSymbols>
        </configuration>
      </execution>
    </executions>
  </plugin>

Integration with sbt
--------------------

`sbt-scalariform`_, written by Olivier Michallat, provides an sbt plugin contributing formatting actions.

.. _sbt-scalariform: http://github.com/olim7t/sbt-scalariform

Integration with TextMate
-------------------------

See Mads Jensen's Scala TextMate bundle:

  http://github.com/mads379/scala.tmbundle

Reformat using Ctrl-Shift-H.

Use with Vim
------------

While there is no specific Vim integration at present, you can use
Scalariform as an external formatter for the ``gq`` command by adding
the following to ``.vimrc`` ::

  au BufEnter *.scala setl formatprg=scala\ -cp\ /path/to/scalariform_2.8.0-0.0.9.jar\ scalariform.commandline.Main\ --forceOutput

Command line tool
-----------------

Scalariform includes a stand-alone command line utility. Sample script::

  #!/bin/bash
  scala -cp /path/to/scalariform-0.0.9.jar scalariform.commandline.Main "$@"

Usage::

  Usage: scalariform [options] [files...]
  
  Options:
    --encoding=<encoding>                Set the encoding, e.g. UTF-8. If not set, defaults to the platform default encoding.
    --fileList=<path>, -l=<path>         Read the list of input file(s) from a text file (one per line)
    --help, -h                           Show help
    --inPlace, -i                        Replace the input file(s) in place with a formatted version.
    --preferenceFile=<path>, -p=<path>   Read preferences from a properties file
    --test, -t                           Check the input(s) to see if they are correctly formatted, return a non-zero error code if not.
    --forceOutput, -f                    Return the input unchanged if the file cannot be parsed correctly. (Only works for input on stdin)
    --verbose, -v                        Verbose output
    --version                            Show Scalariform version
  
  Preferences:
    [+|-]alignParameters                                  Enable/disable Align parameters on different lines in the same column
    [+|-]alignSingleLineCaseStatements                    Enable/disable Align the arrows of consecutive single-line case statements
    [+|-]compactStringConcatenation                       Enable/disable Omit spaces when formatting a '+' operator on String literals
    [+|-]doubleIndentClassDeclaration                     Enable/disable Double indent either a class's parameters or its inheritance list
    [+|-]formatXml                                        Enable/disable Format XML literals
    [+|-]indentLocalDefs                                  Enable/disable Indent local defs an extra level
    [+|-]indentPackageBlocks                              Enable/disable Indent package blocks
    [+|-]preserveDanglingCloseParenthesis                 Enable/disable Allow a newline before a ')' in an argument expression.
    [+|-]preserveSpaceBeforeArguments                     Enable/disable Preserve a space before a parenthesis argument
    [+|-]rewriteArrowSymbols                              Enable/disable Replace arrow tokens with unicode equivalents: => with ⇒, and <- with ←
    [+|-]spaceBeforeColon                                 Enable/disable Add a space before colons
    [+|-]spaceInsideBrackets                              Enable/disable Require a space after '[' and before ']'
    [+|-]spaceInsideParentheses                           Enable/disable Require a space after '(' and before ')'
    [+|-]spacesWithinPatternBinders                            Enable/disable Add a space around the @ token in pattern binders
    -alignSingleLineCaseStatements.maxArrowIndent=[1-100] Set Maximum number of spaces inserted before an arrow to align case statements
    -indentSpaces=[1-10]                                  Set Number of spaces to use for indentation
  
  Examples:
   scalariform +spaceBeforeColon -alignParameters -indentSpaces=2 --inPlace foo.scala
   find . -name '*.scala' | xargs scalariform +rewriteArrowSymbols --verbose --test
   echo 'class A ( n  :Int )' | scalariform

Library
-------

Example usage::

  import scalariform.formatter.preferences._
  import scalariform.formatter.ScalaFormatter
  import scalariform.parser.ScalaParserException
  
  object Test extends Application {
  
    val unformattedScala = """
      class A  {
      println (42)}"""
    val preferences = FormattingPreferences().setPreference(IndentSpaces, 3)
    try {
      val formattedScala = ScalaFormatter.format(unformattedScala, preferences)
      println(formattedScala)
    } catch {
       case e: ScalaParserException => println("Syntax error in Scala source")
    }
  
  }

Preferences
-----------

alignParameters
~~~~~~~~~~~~~~~

Default: ``false``

Align class/function parameters in the same column. For example, if ``false``, then::

  class Person(name: String,
    age: Int,
    birthdate: Date,
    astrologicalSign: String,
    shoeSize: Int,
    favoriteColor: java.awt.Color)

If ``true``, then::

  class Person(name: String,
               age: Int,
               birthdate: Date,
               astrologicalSign: String,
               shoeSize: Int,
               favoriteColor: java.awt.Color)

alignSingleLineCaseStatements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Default: ``false``

Align the arrows of consecutive single-line case statements. For example, if ``true``, then::

  a match {
    case b => 1
    case ccc => 2
    case dd => 3
  }

Is reformatted as::

  a match {
    case b   => 1
    case ccc => 2
    case dd  => 3
  }

alignSingleLineCaseStatements.maxArrowIndent
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Default: ``40``

When using alignSingleLineCaseStatements == true, this is a limit on
the number of spaces that can be inserted before an arrow to align it
with other case statements. This can be used to avoid very large gaps,
e.g.::

  a match {
    case Some(wibble, wobble) if wibble + wibble > wobble * wibble => 1
    case ccc                                                       => 2
  }


compactStringConcatenation
~~~~~~~~~~~~~~~~~~~~~~~~~~

Default: ``false``

Omit spaces when formatting a '+' operator on String literals". For example, If ``false``, then::

  "Hello " + name + "!"

If ``true``, then::

  "Hello "+name+"!"

The Scala Style Guide recommends_ that operators, "should `always` be
invoked using infix notation with spaces separated the target".

.. _recommends: http://davetron5000.github.com/scala-style/method_invocation/operators.html

doubleIndentClassDeclaration
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Default: ``false``

With this set to ``true``, class (and trait / object) declarations
will be formatted as recommended by the `Scala Style Guide`_. That is,
if the declaration section spans multiple lines, it will be formatted
so that either the parameter section or the inheritance section is
doubly indented. This provides a visual distinction from the members
of the class. For example::

  class Person(
    name: String,
    age: Int,
    birthdate: Date,
    astrologicalSign: String,
    shoeSize: Int,
    favoriteColor: java.awt.Color)
      extends Entity
      with Logging
      with Identifiable
      with Serializable {
    def firstMethod = ...
  }

Or::

  class Person(
      name: String,
      age: Int,
      birthdate: Date,
      astrologicalSign: String,
      shoeSize: Int,
      favoriteColor: java.awt.Color) {
    def firstMethod = ...
  }

formatXml
~~~~~~~~~

Default: ``true``

Format embedded XML literals; if ``false`` they will be left untouched.

indentLocalDefs
~~~~~~~~~~~~~~~

Default: ``false``

If ``true``, indent local methods an extra level, with the intention of distinguishing them from other statements. For example,::

  class A {
    def find(...) = {
      val x = ...
        def find0() = {
          ...
        }
      find0(...)
    }
  }


indentPackageBlocks
~~~~~~~~~~~~~~~~~~~

Default: ``true``

Whether to indent package blocks. For example, if ``true``::

  package foo {
    package bar {
      class Baz
    }
  }

Else if ``false``::

  package foo {
  package bar {
  class Baz
  }
  }

indentSpaces
~~~~~~~~~~~~

Default: ``2``

The number of spaces to use for each level of indentation.

preserveDanglingCloseParenthesis
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Default: ``false``

If ``true``, it will keep a newline before a close parenthesis ')' in an
argument expression. For example::

  val book = Book(
    name = "Name",
    author = "Author",
    rating = 5
  )

If ``false``, the parenthesis will be joined to the end of the argument list::

  val book = Book(
    name = "Name",
    author = "Author",
    rating = 5)

preserveSpaceBeforeArguments
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Default: ``false``

If ``true``, the formatter will keep an existing space before a parenthesis argument. For example::

  stack.pop() should equal (2)

Otherwise, if ``false``, spaces before arguments will always be removed.

rewriteArrowSymbols
~~~~~~~~~~~~~~~~~~~

Default: ``false``

Replace arrow tokens with their unicode equivalents: ``=>`` with ``⇒``, and ``<-`` with ``←``. For example::

  for (n <- 1 to 10) n % 2 match {
    case 0 => println("even")
    case 1 => println("odd")
  }

is formatted as::

  for (n ← 1 to 10) n % 2 match {
    case 0 ⇒ println("even")
    case 1 ⇒ println("odd")
  }

spaceBeforeColon
~~~~~~~~~~~~~~~~

Default: ``false``

Whether to ensure a space before colon. For example, If ``false``, then::

  def add(a: Int, b: Int): Int = a + b

If ``true``, then::

  def add(a : Int, b : Int) : Int = a + b

spaceInsideBrackets
~~~~~~~~~~~~~~~~~~~

Default: ``false``

Whether to use a space inside type brackets. For example, if ``true``, then::

  Array[ String ]

If ``false``, then::

  Array[String]

spaceInsideParentheses
~~~~~~~~~~~~~~~~~~~~~~

Default: ``false``

Whether to use a space inside non-empty parentheses. For example, if ``true``, then::

  def main( args : Array[String] )

If ``false``, then::

  def main(args : Array[String])

spacesWithinPatternBinders
~~~~~~~~~~~~~~~~~~~~~~~~~~

Default: ``true``

Whether to add a space around the @ token in pattern binders. For example, if ``true``,::

  case elem @ Multi(values @ _*) =>

If ``false``,::

  case elem@Multi(values@_*) =>


Scala Style Guide
~~~~~~~~~~~~~~~~~

Scalariform is compatible with the `Scala Style Guide`_ in the sense
that, given the right preference settings, source code that is
initially compiliant with the Style Guide will not become uncompliant
after formatting. In a number of cases, running the formatter will
make uncompliant source more compliant.

============================ ========= =========
Preference                   Value     Default?
============================ ========= =========
alignParameters              ``false`` 
compactStringConcatenation   ``false`` 
doubleIndentClassDeclaration ``true``    No
indentSpaces                 ``2``       
preserveSpaceBeforeArguments ``false`` 
rewriteArrowSymbols          ``false`` 
spaceBeforeColon             ``false`` 
spaceInsideBrackets          ``false``
spaceInsideParentheses       ``false``
============================ ========= =========

Source directives
-----------------

As well as global preferences, formatting can be tweaked at the source level through comments.

format: [ON|OFF]
~~~~~~~~~~~~~~~~

Disables the formatter for selective portions of a source file::

  // format: OFF    <-- this directive disables formatting from this point
  class AsciiDSL { 
    n ¦- "1" -+ { n: Node =>
            n ¦- "i"  
            n ¦- "ii"  
            n ¦- "iii"  
            n ¦- "iv"  
            n ¦- "v"
    }
    n ¦- "2"
    n ¦- "3" -+ { n: Node =>
            n ¦- "i"  
            n ¦- "ii" -+ { n: Node =>
                     n ¦- "a"
                     n ¦- "b"
                     n ¦- "c"
            }
            n ¦- "iii"  
            n ¦- "iv"  
            n ¦- "v"
    }
    // format: ON   <-- formatter resumes from this point
    ...
  }
  // (see: http://dev.day.com/microsling/content/blogs/main/scalajcr2.html)

format: [+|-]<preferenceName>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Sets a preference for the entire of the source file, overriding the global formatting settings::

  // format: +preserveSpaceBeforeArguments
  class StackSpec extends FlatSpec with ShouldMatchers {
    // ...
    stack.pop() should equal (2)
  }
