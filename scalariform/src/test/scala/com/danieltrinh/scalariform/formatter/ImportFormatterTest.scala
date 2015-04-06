package com.danieltrinh.scalariform.formatter

import com.danieltrinh.scalariform.parser._
import com.danieltrinh.scalariform.formatter._
import com.danieltrinh.scalariform.formatter.preferences.{SpacesAroundMultiImports, FormattingPreferences}

// format: OFF
class ImportFormatterTest extends AbstractFormatterTest {

  "import foo . _" ==> "import foo._" 
  "import foo . bar" ==> "import foo.bar" 
  "import foo.{bar=>baz}" ==> "import foo.{ bar => baz }"
  "import foo.{bar=>baz},baz.biz" ==> "import foo.{ bar => baz }, baz.biz"

  """import foo.{bar => baz,
    |wibble => wobble}""" ==>
  """import foo.{
    |  bar => baz,
    |  wibble => wobble
    |}"""


  {
    implicit val formattingPreferences = FormattingPreferences.setPreference(SpacesAroundMultiImports, false)

    "import foo.{bar=>baz}" ==> "import foo.{bar => baz}"
    "import foo.{bar=>baz},baz.biz" ==> "import foo.{bar => baz}, baz.biz"
  }

  override val debug = false

  type Result = CompilationUnit

  def parse(parser: ScalaParser) = parser.compilationUnit()
  
  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState())

}
