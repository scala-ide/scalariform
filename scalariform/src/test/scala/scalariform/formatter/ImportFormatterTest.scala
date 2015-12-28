package scalariform.formatter

import scalariform.parser._
import scalariform.formatter.preferences.{SpacesAroundMultiImports, FormattingPreferences}

// format: OFF
class ImportFormatterTest extends AbstractFormatterTest {

  {
    implicit val formattingPreferences = FormattingPreferences.setPreference(
      SpacesAroundMultiImports, true)

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
  }

  {
    implicit val formattingPreferences = FormattingPreferences.setPreference(
      SpacesAroundMultiImports, false)

    "import foo . _" ==> "import foo._"
    "import foo . bar" ==> "import foo.bar"
    "import foo.{bar=>baz}" ==> "import foo.{bar => baz}"
    "import foo.{bar=>baz},baz.biz" ==> "import foo.{bar => baz}, baz.biz"
    """import foo.{bar => baz,
      |wibble => wobble}""" ==>
      """import foo.{
        |  bar => baz,
        |  wibble => wobble
        |}"""
  }

  override val debug = false

  type Result = CompilationUnit

  def parse(parser: ScalaParser) = parser.compilationUnit()

  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState())

}
