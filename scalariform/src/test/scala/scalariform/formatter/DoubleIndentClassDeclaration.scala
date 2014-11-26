package scalariform.formatter

import scalariform.formatter.preferences._
import scalariform.parser._

class DoubleIndentClassDeclaration extends AbstractFormatterTest {

  override val debug = false

  def parse(parser: ScalaParser) = parser.nonLocalDefOrDcl()

  type Result = FullDefOrDcl

  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState(indentLevel = 0))

  implicit val formattingPreferences = FormattingPreferences.setPreference(DoubleIndentClassDeclaration, true)
  """class Person(
    |  name: String,
    |  age: Int)
    |    extends Entity
    |    with Logging
    |    with Identifiable
    |    with Serializable""" ==>
  """class Person(
    |    name: String,
    |    age: Int
    |)
    |  extends Entity
    |  with Logging
    |  with Identifiable
    |  with Serializable"""

  """class Person(
    |    name: String,
    |    age: Int) {
    |  def firstMethod = 42
    |}""" ==>
  """class Person(
    |    name: String,
    |    age: Int
    |) {
    |  def firstMethod = 42
    |}"""

  """class Person(name: String, age: Int, birthdate: Date, astrologicalSign: String, shoeSize: Int, favoriteColor: java.awt.Color)
    |extends Entity
    |with Logging
    |with Identifiable
    |with Serializable {
    |def firstMethod = 42
    |}""" ==>
  """class Person(name: String, age: Int, birthdate: Date, astrologicalSign: String, shoeSize: Int, favoriteColor: java.awt.Color)
    |  extends Entity
    |  with Logging
    |  with Identifiable
    |  with Serializable {
    |  def firstMethod = 42
    |}"""

  """class Person(
    |name: String,
    |  age: Int)
    |extends Entity  {
    |def method() = 42
    |}""" ==>
  """class Person(
    |    name: String,
    |    age: Int
    |)
    |  extends Entity {
    |  def method() = 42
    |}"""

  """trait A
    |extends B
    |with C {
    |println("d")
    |}""" ==>
  """trait A
    |  extends B
    |  with C {
    |  println("d")
    |}"""

  """class Person(
    |  name: String,
    |  age: Int)
    |    extends Entity with Logging""" ==>
  """class Person(
    |    name: String,
    |    age: Int
    |)
    |  extends Entity with Logging"""

}

