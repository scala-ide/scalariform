Scalariform
===========

Scalariform is a code formatter for Scala 2.8. It is a stand-alone
library and an Eclipse plug-in. Currently, Scalariform supports only a
limited set of options, although it is intended to be compatible with
the recommendations of the `Scala Style Guide`_ (see below). Please
let me know what other features people would like.

Scalariform is licenced under `The MIT Licence`_.

.. _Scala Style Guide: http://davetron5000.github.com/scala-style/
.. _The MIT Licence: http://www.opensource.org/licenses/mit-license.php

Installation
------------

In Eclipse Ganymede (3.5), install the latest nightly Scala Eclipse
plugin using the Eclipse update site mechanism:

  http://www.scala-lang.org/scala-eclipse-plugin-nightly

(See http://www.scala-lang.org/node/94 for more detailed instructions.)

Then install Scalariform from this update site:

  http://scalariform.googlecode.com/svn/trunk/update-site/

To format:

- Right click in the editor -> Format Scala Source Code, or 
- Press Ctrl-Shift-D

To configure preferences, go to Window -> Preferences -> Scala -> Scala Formatter Preferences

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
