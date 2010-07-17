Scalariform
===========

Scalariform is a code formatter for Scala 2.8. It is both a
stand-alone command line tool / library and an Eclipse
plug-in. Currently, Scalariform supports only a limited set of
options, although it is intended to be compatible with the
recommendations of the `Scala Style Guide`_ (see below). Please let me
know what other features people would like.

Scalariform is licenced under `The MIT Licence`_.

.. _Scala Style Guide: http://davetron5000.github.com/scala-style/
.. _The MIT Licence: http://www.opensource.org/licenses/mit-license.php

Use with Eclipse
----------------

In Eclipse 3.5.2, install the ``tycho-reorg`` build of Scala IDE,
which integrates Scalariform as the formatter. You can use the
following update site:

  http://download.scala-ide.org/nightly-update-tycho-reorg-2.8.0.RC6/

(See http://www.assembla.com/wiki/show/scala-ide/Requirements_and_Installation for more detailed instructions.)

Formatting works the same in the Scala editor; that is, either

- Right click in the editor -> Source -> Format
- Press Ctrl-Shift-F

To configure preferences, go to Window -> Preferences -> Scala -> Formatter

It can also perform formatting as a save action (Window -> Preferences -> Java -> Editor -> Save Actions).

Command line tool
-----------------

Scalariform can be used as a stand-alone command line utility. Sample script::

  #!/bin/bash
  scala -cp /path/to/scalariform-X.Y.Z.jar scalariform.commandline.Main "$@"

Usage::

  scalariform [options] [files...]
  
  Options:
    --help, -h                      Show help
    --inPlace, -i                   Replace the input file(s) in place with a formatted version.
    --test, -t                      Check the input(s) to see if they are correctly formatted, return a non-zero error code if not.
    --fileList=<path>, -l=<path>    Read the list of input file(s) from a text file (one per line)
    --verbose -v                    Verbose output
  
  Preferences:
    [+|-]alignParameters                Enable/disable Align parameters on different lines in the same column
    [+|-]compactStringConcatenation     Enable/disable Omit spaces when formatting a '+' operator on String literals
    [+|-]doubleIndentClassDeclaration   Enable/disable Double indent either a class's parameters or its inheritance list
    [+|-]formatXml                      Enable/disable Format XML literals
    -indentSpaces=[1-10]                Set Number of spaces to use for indentation
    [+|-]preserveSpaceBeforeArguments   Enable/disable Preserve a space before a parenthesis argument
    [+|-]rewriteArrowSymbols            Enable/disable Replace arrow tokens with unicode equivalents: => with ⇒, and <- with ←
    [+|-]spaceBeforeColon               Enable/disable Add a space before colons
  
  Examples:
   scalariform +spaceBeforeColon -alignParameters -indentSpaces=2 --inPlace foo.scala
   find . -name '*.scala' | xargs scalariform +rewriteArrowSymbols --verbose --test
   echo 'class A ( n  :Int )' | scalariform

Integration with sbt
--------------------

`sbt-scalariform`_, written by Olivier Michallat, provides an sbt plugin contributing formatting actions.

.. _sbt-scalariform: http://github.com/olim7t/sbt-scalariform

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

indentSpaces 
~~~~~~~~~~~~

Default: ``2``

The number of spaces to use for each level of indentation.

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

Scala Style Guide
~~~~~~~~~~~~~~~~~

Scalariform is "compatible" with the `Scala Style Guide`_ v1.1.0 in the
sense that, given the right preference settings, source code that is
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
