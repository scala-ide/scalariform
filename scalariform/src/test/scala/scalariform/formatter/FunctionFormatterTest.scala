package scalariform.formatter

import scalariform.parser.{CompilationUnit, ScalaParser}

// format: OFF
class FunctionFormatterTest extends AbstractFormatterTest {

  "val f = x   =>  x" ==> "val f = x => x"
  "val f: Int => Int => Unit = a => b => ()" ==> "val f: Int => Int => Unit = a => b => ()"

  "{ ctx => j => () }" ==> "{ ctx => j => () }"
  "fun { ctx => j => () }" ==> "fun { ctx => j => () }"
  "Thing() { ctx => j => () }" ==> "Thing() { ctx => j => () }"

  """{ ctx =>
    |  ???
    |}""".stripMargin ==>
  """{ ctx =>
    |  ???
    |}""".stripMargin

  """{ ctx => j =>
    |  ???
    |}""".stripMargin ==>
  """{ ctx => j =>
    |  ???
    |}""".stripMargin

  """val  x  = {  ctx   =>   j   =>
    |    one()
    |      two()
    |}""".stripMargin ==>
  """val x = { ctx => j =>
    |  one()
    |  two()
    |}""".stripMargin

  override val debug = false

  type Result = CompilationUnit

  def parse(parser: ScalaParser) = parser.compilationUnitOrScript()

  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState())

}
