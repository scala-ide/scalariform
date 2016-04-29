Scalariform
===========

Scalariform is a code formatter for Scala. It's available as a
library, a stand-alone command line tool, or via integrations with
various editors and build tools (listed below).

The plan is to add preferences and features as and when people ask for
them, so please do raise a Github issue if it doesn't format your code
the way you'd like it, and I'll see what I can do.

Scalariform is licenced under `The MIT Licence`_.

.. _Scala Style Guide: http://docs.scala-lang.org/style/
.. _The MIT Licence: http://opensource.org/licenses/mit-license.php

Installing with Homebrew (for OS X users)
-----------------------------------------

Mac OS X users can install the scalariform CLI tool using the `Homebrew`_ package manager. ::

    brew install scalariform

Or, if you would like to install the latest development release: ::

    brew install --HEAD scalariform

.. _Homebrew: https://github.com/Homebrew/homebrew

Packaging an executable JAR
---------------------------

If you would like to package scalariform for use on the command line with java -jar, clone the repo and perform the following simple steps: ::

    sbt "project cli" "assembly"

sbt will build one jar with all the dependencies and put it in ::

    cli/target/scala-$your_scala_version/cli-assembly-$scalariform_version.jar

You can copy this to a location in your path and execute it as follows: ::

   java -jar /home/me/bin/cli-assembly-$scalariform_version.jar -f -q +compactControlReadability +alignParameters +alignSingleLineCaseStatements +doubleIndentClassDeclaration +preserveDanglingCloseParenthesis +rewriteArrowSymbols +preserveSpaceBeforeArguments --stdout ~/myproject/src/main/scala/Stuff.scala > Stuff.scala

Integration with sbt
--------------------

A plugin for SBT is available at https://github.com/sbt/sbt-scalariform.

Usage within a project
----------------------

Have a use for the scalariform source code directly? You can use it as a build dependency: ::

    "org.scalariform" %% "scalariform" % "0.1.8"

Integration with Eclipse
------------------------

Scala IDE for Eclipse uses Scalariform for code formatting:

- Right click in the editor -> Source -> Format
- Press Ctrl-Shift-F

If you select some lines, only those will be formatted.

You can also configure formatting to be run as a save action (Window -> Preferences -> Java -> Editor -> Save Actions).

To set preferences, go to either

- Window -> Preferences -> Scala -> Editor -> Formatter
- Project -> Properties -> Scala Formatter

Integration with Emacs/ENSIME
-----------------------------

"`ENSIME`_ uses the Scalariform library to format Scala sources. Type C-c C-v f to format the current buffer."

.. _ENSIME: https://github.com/ensime/ensime-server

Integration with jEdit
----------------------

See `ScalaSidekick`_ by Stefan Ettrup:

.. _ScalaSidekick: https://github.com/StefanE/ScalaSidekick

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
    <version>0.1.4</version>
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

Integration with Gradle
-----------------------

There is a `Gradle plugin`_ to run Scalariform contributed by Jeroen van Erp.

.. _Gradle plugin: https://github.com/hierynomus/scalariform-gradle-plugin

Usage (Gradle 2.1 and above)::

  plugins {
    id "com.github.hierynomus.scalariform" version "0.1.0"
  }

  // optionally, configure Scalariform settings
  scalariform {
    alignParameters = true
    alignSingleLineCaseStatements = true
  }
  
  formatAllScala

See `the documentation`_ for further usage examples.

.. _the documentation: https://github.com/hierynomus/scalariform-gradle-plugin/blob/master/README.adoc

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

  au BufEnter *.scala setl formatprg=java\ -jar\ /home/me/bin/scalariform.jar\ -f\ -q\ +compactControlReadability\ +alignParameters\ +alignSingleLineCaseStatements\ +doubleIndentClassDeclaration\ +preserveDanglingCloseParenthesis\ +rewriteArrowSymbols\ +preserveSpaceBeforeArguments\ --stdin\ --stdout

Or, if you don't like escaping spaces, you can set up a mapping: ::

    map ,st :%!java -jar /home/me/bin/scalariform.jar -f -q +compactControlReadability +alignParameters +alignSingleLineCaseStatements +doubleIndentClassDeclaration +preserveDanglingCloseParenthesis +rewriteArrowSymbols +preserveSpaceBeforeArguments --stdin --stdout <CR>

You can create your own executable scalariform.jar by following the instructions at the top of this file, in "Packaging an executable JAR."

Command line tool
-----------------

  https://github.com/scala-ide/scalariform/wiki/Command-line-tool

Library
-------

  https://github.com/scala-ide/scalariform/wiki/Library

Preferences
-----------

alignParameters
~~~~~~~~~~~~~~~

Default: ``false``

Align class/function parameters (modifiers and name, type, and defaults) in three columns.

For example, if ``false``, then:

.. code:: scala

  class Person(name: String,
    age: Int = 24,
    birthdate: Date,
    astrologicalSign: String = "libra",
    shoeSize: Int,
    favoriteColor: java.awt.Color
  )

If ``true``, then:

.. code:: scala

  class Person(
    name:             String,
    age:              Int            = 24,
    birthdate:        Date,
    astrologicalSign: String         = "libra",
    shoeSize:         Int,
    favoriteColor:    java.awt.Color
  )

This will also place the "implicit" keyword in parameters on its own line, whenever
the parameter being formatted contains a newline::

For example, if ``false``, then:

.. code:: scala

  def formatBirthDate(
    implicit birthdate: Date = Date("11/11/11"),
    birthtime: Time
  ): DateTime

If ``true``, then:

.. code:: scala

  def formatBirthDate(
    implicit
    birthdate: Date = Date("11/11/11"),
    birthtime: Time
  ): DateTime

This option is disabled if ``indentWithTabs`` is ``true``.


alignArguments
~~~~~~~~~~~~~~

Default: ``false``

Aligns multi-line arguments

For example, if ``false``, then:

.. code:: scala

  Cake(candles = 10,
    frostingFlavor = Vanilla,
    layerFlavor = Chocolate,
    iceCream = true
  )

If ``true``, then:

.. code:: scala

  Cake(
    candles        = 10,
    frostingFlavor = Vanilla,
    layerFlavor    = Chocolate,
    iceCream       = true
  )

This option is disabled if ``indentWithTabs`` is ``true``.


alignSingleLineCaseStatements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Default: ``false``

Align the arrows of consecutive single-line case statements. For example, if ``true``, then:

.. code:: scala

  a match {
    case b => 1
    case ccc => 2
    case dd => 3
  }

Is reformatted as:

.. code:: scala

  a match {
    case b   => 1
    case ccc => 2
    case dd  => 3
  }

This option is disabled if ``indentWithTabs`` is ``true``.

alignSingleLineCaseStatements.maxArrowIndent
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Default: ``40``

When ``alignSingleLineCaseStatements`` is ``true``, this is a limit on
the number of spaces that can be inserted before an arrow to align it
with other case statements. This can be used to avoid very large gaps,
e.g.:

.. code:: scala

  a match {
    case Some(wibble, wobble) if wibble + wibble > wobble * wibble => 1
    case ccc                                                       => 2
  }

compactControlReadability
~~~~~~~~~~~~~~~~~~~~~~~~~

Default: ``false``

When ``compactControlReadability`` is ``true``, then ``if``/``else`` and
``try``/``catch``/``finally`` control structures will be formatted
using `Compact Control Readability`_ style

.. _Compact Control Readability: https://en.wikipedia.org/wiki/Indent_style#Variant:_Stroustrup

.. code:: scala

  if (x == y) {
    foo()
  }
  else if (y == z) {
    bar()
  }
  else {
    baz()
  }

  try {
    foo()
  }
  catch {
    case _ => bar()
  }
  finally {
    baz()
  }


compactStringConcatenation
~~~~~~~~~~~~~~~~~~~~~~~~~~

Default: ``false``

Omit spaces when formatting a '+' operator on String literals. For example, if ``false``, then:

.. code:: scala

  "Hello " + name + "!"

If ``true``, then:

.. code:: scala

  "Hello "+name+"!"

The Scala Style Guide recommends_ that operators, "should `always` be
invoked using infix notation with spaces separated the target".

.. _recommends: http://docs.scala-lang.org/style/method-invocation.html#symbolic-methodsoperators

doubleIndentClassDeclaration
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Default: ``false``

With this set to ``true``, class (and trait / object) declarations
will be formatted as recommended_ by the Scala Style Guide. That is,
if the declaration section spans multiple lines, it will be formatted
so that either the parameter section or the inheritance section is
doubly indented. This provides a visual distinction from the members
of the class. For example:

.. code:: scala

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

Or:

.. code:: scala

  class Person(
      name: String,
      age: Int,
      birthdate: Date,
      astrologicalSign: String,
      shoeSize: Int,
      favoriteColor: java.awt.Color) {
    def firstMethod = ...
  }

.. _recommended: http://docs.scala-lang.org/style/declarations.html#classes

doubleIndentMethodDeclaration
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Default: ``false``

With this set to ``true``, method declarations will have an extra indentation
added to their parameter list, if it spans multiple lines.
This provides a visual distinction from the method body. For example::

  def longMethodNameIsLong(paramOneNameIsLong: String, paramTwo: String,
      paramThreeNameIsReallyLong): Unit = {
    val startOfMethod = ...
  }

Or::

  def longMethodNameIsLong(
      paramOneNameIsLong: String,
      paramTwoNameIsLong: String,
      paramThreeNameIsLong): Unit = {
    val startOfMethod = ...
  }

formatXml
~~~~~~~~~

Default: ``true``

Format embedded XML literals; if ``false`` they will be left untouched.

indentLocalDefs
~~~~~~~~~~~~~~~

Default: ``false``

If ``true``, indent local methods an extra level, with the intention of distinguishing them from other statements. For example,:

.. code:: scala

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

Whether to indent package blocks. For example, if ``true``:

.. code:: scala

  package foo {
    package bar {
      class Baz
    }
  }

Else if ``false``:

.. code:: scala

  package foo {
  package bar {
  class Baz
  }
  }

indentSpaces
~~~~~~~~~~~~

Default: ``2``

The number of spaces to use for each level of indentation.

This option is ignored if ``indentWithTabs`` is ``true``.

indentWithTabs
~~~~~~~~~~~~~~

Default: ``false``

Use a tab for each level of indentation. When set to ``true``, this
ignores any setting given for ``indentSpaces``. In addition, for the
moment, ``alignSingleLineCaseStatements``, ``alignArguments``, and ``alignParameters``
options are not supported when indenting with tabs, and XML
indentation is handled differently.

multilineScaladocCommentsStartOnFirstLine
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Default: ``false``

If ``true``, start a multi-line Scaladoc comment body on same line as the opening comment delimiter:

.. code:: scala

  /** This method applies f to each
   *  element of the given list.
   */

If ``false``, start the comment body on a separate line below the opening delimiter:

.. code:: scala

  /**
   * This method applies f to each
   * element of the given list.
   */

newlineAtEndOfFile
~~~~~~~~~~~~~~~~~~

Default: ``false``

If ``true``, newlines will be added at the end of all formatted files.

placeScaladocAsterisksBeneathSecondAsterisk
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Default: ``false``

If ``true``, Scaladoc asterisks will be placed beneath the second asterisk:

.. code:: scala

  /** Wibble
    * wobble
    */
  class A

Otherwise, if ``false``, beneath the first asterisk:

.. code:: scala

  /** Wibble
   *  wobble
   */
  class A

preserveSpaceBeforeArguments
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Default: ``false``

If ``true``, the formatter will keep an existing space before a parenthesis argument. For example:

.. code:: scala

  stack.pop() should equal (2)

Otherwise, if ``false``, spaces before arguments will always be removed.

danglingCloseParenthesis
~~~~~~~~~~~~~~~~~~~~~~~~

Default: ``Force``

If ``Force``, any closing parentheses will be set to dangle. For example:

.. code:: scala

   Box(
     contents: List[Thing])

becomes:

.. code:: scala

   Box(
     contents: List[Thing]
   )

If ``Prevent``, all dangling parenthesis are collapsed. For example:

.. code:: scala

   Box(
     contents: List[Thing]
   )

becomes:

.. code:: scala

   Box(
     contents: List[Thing])

If ``Preserve``, scalariform will try to match what unformatted source code is already doing per parenthesis,
either forcing or preventing.

rewriteArrowSymbols
~~~~~~~~~~~~~~~~~~~

Default: ``false``

Replace arrow tokens with their unicode equivalents: ``=>`` with ``⇒``, and ``<-`` with ``←``. For example:

.. code:: scala

  for (n <- 1 to 10) n % 2 match {
    case 0 => println("even")
    case 1 => println("odd")
  }

is formatted as:

.. code:: scala

  for (n ← 1 to 10) n % 2 match {
    case 0 ⇒ println("even")
    case 1 ⇒ println("odd")
  }

spaceBeforeColon
~~~~~~~~~~~~~~~~

Default: ``false``

Whether to ensure a space before colon. For example, if ``false``, then:

.. code:: scala

  def add(a: Int, b: Int): Int = a + b

If ``true``, then:

.. code:: scala

  def add(a : Int, b : Int) : Int = a + b

spaceInsideBrackets
~~~~~~~~~~~~~~~~~~~

Default: ``false``

Whether to use a space inside type brackets. For example, if ``true``, then:

.. code:: scala

  Array[ String ]

If ``false``, then:

.. code:: scala

  Array[String]

spaceInsideParentheses
~~~~~~~~~~~~~~~~~~~~~~

Default: ``false``

Whether to use a space inside non-empty parentheses. For example, if ``true``, then:

.. code:: scala

  def main( args : Array[String] )

If ``false``, then:

.. code:: scala

  def main(args : Array[String])

spacesWithinPatternBinders
~~~~~~~~~~~~~~~~~~~~~~~~~~

Default: ``true``

Whether to add a space around the @ token in pattern binders. For example, if ``true``,:

.. code:: scala

  case elem @ Multi(values @ _*) =>

If ``false``,:

.. code:: scala

  case elem@Multi(values@_*) =>

spacesAroundMultiImports
~~~~~~~~~~~~~~~~~~~~~~~~

Default: ``false``

Whether or not to add spaces around multi-imports.
For example, if ``false``, then:

.. code:: scala

  import a.{b,c,d}
  import foo.{bar => baz}

If ``true``, then:

.. code:: scala

  import a.{ b, c, d }
  import foo.{ bar => baz }

Older versions of `Scalariform` used ``true``,
but the standard Scala formatting requires ``false``.

See the examples given in "Chapter 13 - Packages and Imports.", page 244 of *Programming in Scala*
2nd ed. (2010) by Odersky, Spoon and Venners.

Scala Style Guide
~~~~~~~~~~~~~~~~~

Scalariform is compatible with the `Scala Style Guide`_ in the sense
that, given the right preference settings, source code that is
initially compliant with the Style Guide will not become uncompliant
after formatting. In a number of cases, running the formatter will
make uncompliant source more compliant.

=========================================== ========= =========
Preference                                  Value     Default?
=========================================== ========= =========
alignParameters                             ``false``
compactStringConcatenation                  ``false``
doubleIndentClassDeclaration                ``true``    No
indentSpaces                                ``2``
placeScaladocAsterisksBeneathSecondAsterisk ``true``    No
preserveSpaceBeforeArguments                ``false``
rewriteArrowSymbols                         ``false``
spaceBeforeColon                            ``false``
spaceInsideBrackets                         ``false``
spaceInsideParentheses                      ``false``
spacesAroundMultiImports                    ``false``
=========================================== ========= =========

Source Directives
-----------------

As well as global preferences, formatting can be tweaked at the source level through comments.

format: [ON|OFF]
~~~~~~~~~~~~~~~~

Disables the formatter for selective portions of a source file:

.. code:: scala

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

Sets a preference for the entirety of the source file, overriding the global formatting settings:

.. code:: scala

  // format: +preserveSpaceBeforeArguments
  class StackSpec extends FlatSpec with ShouldMatchers {
    // ...
    stack.pop() should equal (2)
  }

