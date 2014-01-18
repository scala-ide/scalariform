Scalariform
===========

Scalariform is a code formatter for Scala. It's available as a
library, a stand-alone command line tool, or via integrations with
various editors and build tools (listed below).

The plan is to add preferences and features as and when people ask for
them, so please do raise a Github issue if it doesn't format your code
the way you'd like it, and I'll see what I can do.

Scalariform is licenced under `The MIT Licence`_.

.. _Scala Style Guide: http://davetron5000.github.com/scala-style/
.. _The MIT Licence: http://www.opensource.org/licenses/mit-license.php

Integration with Eclipse
------------------------

Scala IDE for Eclipse uses Scalariform for code formatting:

- Right click in the editor -> Source -> Format
- Press Ctrl-Shift-F

If you select some lines, only those will be formatted.

You can also configure formatting to be run as a save action (Window -> Preferences -> Java -> Editor -> Save Actions).

To set preferences, go to Window -> Preferences -> Scala -> Formatter

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

Integration with sbt
--------------------

`sbt-scalariform`_, written by Olivier Michallat, provides an sbt plugin contributing formatting actions for sbt 0.7.x.

A version for sbt 0.10.x has been written by Peter Vlugter: https://github.com/typesafehub/sbt-scalariform

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

  au BufEnter *.scala setl formatprg=/path/to/scalariform.jar\ --stdin\ --stdout

The executable scalariform.jar can be downloaded from:

  https://s3.amazonaws.com/scalariform/scalariform.jar

Command line tool
-----------------

  https://github.com/mdr/scalariform/wiki/Command-line-tool

Library
-------

  https://github.com/mdr/scalariform/wiki/Library

Preferences
-----------

alignParameters
~~~~~~~~~~~~~~~

Default: ``false``

Align class/function parameters (modifiers and name, type, and defaults) in three columns.

For example, if ``false``, then::

  class Person(name: String,
    age: Int = 24,
    birthdate: Date,
    astrologicalSign: String = "libra",
    shoeSize: Int,
    favoriteColor: java.awt.Color)

If ``true``, then::

  class Person(name:             String,
               age:              Int            = 24,
               birthdate:        Date,
               astrologicalSign: String         = "libra",
               shoeSize:         Int,
               favoriteColor:    java.awt.Color)

This will also place the "implicit" keyword in parameters on it's own line, whenever
the parameter being formatted contains a newline::

For example, if ``false``, then::

  def formatBirthDate(
    implicit birthdate: Date = Date("11/11/11"),
    birthtime: Time): DateTime

If ``true``, then::

  def formatBirthDate(
    implicit
    birthdate: Date = Date("11/11/11"),
    birthtime: Time): DateTime

This option is disabled if ``indentWithTabs`` is ``true``.

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

This option is disabled if ``indentWithTabs`` is ``true``.

alignSingleLineCaseStatements.maxArrowIndent
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Default: ``40``

When ``alignSingleLineCaseStatements`` is ``true``, this is a limit on
the number of spaces that can be inserted before an arrow to align it
with other case statements. This can be used to avoid very large gaps,
e.g.::

  a match {
    case Some(wibble, wobble) if wibble + wibble > wobble * wibble => 1
    case ccc                                                       => 2
  }

compactControlReadability
~~~~~~~~~~~~~~~~~~~~~~~~~

Default: ``false``

When ``compactControlReadability`` is ``true``, ``if``/``else`` and
``try``/``catch``/``finally`` control structures will be formatted
using `Compact Control Readability`_ style:

.. _Compact Control Readability: http://en.wikipedia.org/wiki/Indent_style#Compact_Control_Readability_style

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

Omit spaces when formatting a '+' operator on String literals. For example, If ``false``, then::

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

This option is ignored if ``indentWithTabs`` is ``true``.

indentWithTabs
~~~~~~~~~~~~~~

Default: ``false``

Use a tab for each level of indentation. When set to ``true``, this
ignores any setting given for ``indentSpaces``. In addition, for the
moment, ``alignSingleLineCaseStatements`` and ``alignParameters``
options are not supported when indenting with tabs, and XML
indentation is handled differently.

multilineScaladocCommentsStartOnFirstLine
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Default: ``false``

If ``true``, start a multi-line Scaladoc comment body on same line as the opening comment delimiter::

  /** This method applies f to each
   *  element of the given list.
   */

If ``false``, start the comment body on a separate line below the opening delimiter::

  /**
   * This method applies f to each
   * element of the given list.
   */

preserveDanglingCloseParenthesis
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Default: ``false``

If ``true``, it will keep a newline before a close parenthesis ')' in an
argument expression or parameter clause. For example::

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


Or with parameters, if ``true``::

  def findBooks(
    author: Option[String] = None,
    title: Option[String] = None
  ): List[Book]

If ``false``::

  def findBooks(
    author: Option[String] = None,
    title: Option[String] = None): List[Book]


placeScaladocAsterisksBeneathSecondAsterisk
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Default: ``false``

If ``true``, Scaladoc asterisks will be placed beneath the second asterisk::

  /** Wibble
    * wobble
    */
  class A

Otherwise, if ``false``, beneath the first asterisk::

  /** Wibble
   *  wobble
   */
  class A

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

============================                ========= =========
Preference                                  Value     Default?
============================                ========= =========
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
============================                ========= =========

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
