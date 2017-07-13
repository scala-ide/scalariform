package scalariform.formatter

import scalariform.parser._
import scalariform.formatter.preferences._

// format: OFF
class TemplateFormatterTest extends AbstractFormatterTest {

"case class A" ==> "case class A"

"""class A extends B {
   |  foo()
   |  bar()
   |}""" ==>
"""class A extends B {
   |  foo()
   |  bar()
   |}"""

"""class A {
   |val n: Int
   |}""" ==>
"""class A {
   |  val n: Int
   |}"""

"class A" ==> "class A"
"trait A" ==> "trait A"

"class A private (val b: C)" ==> "class A private (val b: C)"
"class A [B]" ==> "class A[B]"
"class A[B]private(val c: D)" ==> "class A[B] private (val c: D)"
"class A (val b: C) (val d: E) (implicit val f: G)" ==> "class A(val b: C)(val d: E)(implicit val f: G)"
"class A (implicit val f: G)" ==> "class A(implicit val f: G)"

"abstract class E [F]private(val h: I) (implicit j: K) extends{} with L(2) with M{}" ==>
   "abstract class E[F] private (val h: I)(implicit j: K) extends {} with L(2) with M {}"

"class A{}" ==> "class A {}"

"class A@Deprecated()private (val b: C)" ==> "class A @Deprecated() private (val b: C)"
"class A@Annotation1()@Annotation2()(val b: C)" ==> "class A @Annotation1() @Annotation2() (val b: C)"
"class A[B]@Annotation()private(val c: D)" ==> "class A[B] @Annotation() private (val c: D)"

"class A @Deprecated" ==> "class A @Deprecated"
"class A @Deprecated private" ==> "class A @Deprecated private"
"class A @Deprecated private (n: Int)" ==> "class A @Deprecated private (n: Int)"

"""@A@B(c = "d")abstract class E [F]@G()private(val h: I) (implicit j: K) extends{} with L(2) with M{}""" ==>
"""@A @B(c = "d") abstract class E[F] @G() private (val h: I)(implicit j: K) extends {} with L(2) with M {}"""

"""class Foo {private[Foo]type Bar=String}""" ==>
  """class Foo { private[Foo] type Bar = String }"""

"""@A/*a*/@B
   |/*b*/class E""" =/=>
"""@A/*a*/
   |@B
   |/*b*/
   |class E""" because "of inconsistency between spacing between annotations and comments"

"""/*a*/@A/*b*/@B(c = "d")/*c*/abstract class/*d*/E/*e*/[F]/*f*/@G()/*g*/private/*h*/(val h: I)/*i*/(implicit j: K)/*j*/extends/*k*/{} with/*l*/L(2) with M/*m*/{}""" =/=>
"""/*a*/
   |@A/*b*/
   |@B(c = "d")/*c*/
   |abstract class/*d*/E/*e*/[F]/*f*/@G()/*g*/private/*h*/(val h: I)/*i*/(implicit j: K)/*j*/extends/*k*/{} with/*l*/L(2) with M/*m*/{}""" because "sort out what we want"

{

implicit val formattingPreferences = FormattingPreferences.setPreference(SpacesWithinPatternBinders, true)

"@(Id@Field) class A" ==> "@(Id @Field) class A"

}

  """class A {
    |
    |  class B
    |
    |  protected def c
    |
    |}""" ==>
  """class A {
    |
    |  class B
    |
    |  protected def c
    |
    |}"""

  """class A{
    |(
    |null match {
    |case b => val c = {d: Int => 1}
    |1.toString
    |}
    |)
    |}""" ==>
  """class A {
    |  (
    |    null match {
    |      case b =>
    |        val c = { d: Int => 1 }
    |        1.toString
    |    })
    |}"""

  """class C1492 {
    |
    |  class X
    |
    |  def foo(x: X => X) {}
    |
    |  foo ( implicit x => implicitly[X] )
    |  foo { implicit x => implicitly[X] }
    |}""" ==>
  """class C1492 {
    |
    |  class X
    |
    |  def foo(x: X => X) {}
    |
    |  foo(implicit x => implicitly[X])
    |  foo { implicit x => implicitly[X] }
    |}"""

  """class A {
    | <b/>
    | 42
    |}""" ==>
  """class A {
    |  <b/>
    |  42
    |}"""

  """class A {
    |  val x = new {
    |  def foo = 3
    |  }
    |}""" ==>
  """class A {
    |  val x = new {
    |    def foo = 3
    |  }
    |}"""

  """class A(n: Int){}""" ==>
  """class A(n: Int) {}"""

  """class A {
    |println("B")
    |// Comment
    |}""" ==>
  """class A {
    |  println("B")
    |  // Comment
    |}"""

  """trait A {
    |this: B =>
    |val c
    |}""" ==>
    """trait A {
    |  this: B =>
    |  val c
    |}"""

  """trait A {
    |this: B =>
    |println("foo")
    |}""" ==>
  """trait A {
    |  this: B =>
    |  println("foo")
    |}"""

  "object X0 { 0;  (a : Int, b : Int, c : Int) => println(List(a, b)) }" ==>
    "object X0 { 0; (a: Int, b: Int, c: Int) => println(List(a, b)) }" // Case for self type / anon function literal syntax disambig

  "@serializable class A" ==> "@serializable class A"

  "@volatile var nParticles = 0" ==> "@volatile var nParticles = 0" // Issue #28

  """@volatile
    |var nParticles = 0""" ==>
  """@volatile
    |var nParticles = 0"""

  """class A extends B[C] {
    |println("foo")
    |}""" ==>
  """class A extends B[C] {
    |  println("foo")
    |}"""

  "class A(private val b: C)" ==> "class A(private val b: C)"
  "class A(protected val b: C)" ==> "class A(protected val b: C)"
  "class A(override val b: C)" ==> "class A(override val b: C)"

  """class C[T <: A {val n: Int
    |val m :Int}]""" ==>
  """class C[T <: A {
    |  val n: Int
    |  val m: Int
    |}]"""

  """class C(n: { val n: Int
    |val m: Int} )""" ==>
  """class C(n: {
    |  val n: Int
    |  val m: Int
    |})"""

  """class C(@annotation(foo = {
    |1 + 2}) n: { val n: Int
    |val m: Int} = { val n: Int
    |val m: Int})""" ==>
  """class C(@annotation(foo = {
    |  1 + 2
    |}) n: {
    |  val n: Int
    |  val m: Int
    |} = {
    |  val n: Int
    |  val m: Int
    |})"""

  """class A(b: C)
    |(d: E) """ ==>
  """class A(b: C)(d: E)""" // Maybe handle multiple ParamClauses in future.

  """class A
    |{
    |println("b")
    |}""" ==>
  """class A {
    |  println("b")
    |}"""

  """class A extends B{
    |c
    |}""" ==>
  """class A extends B {
    |  c
    |}"""

  """class A extends B(() => {
    |  if (a)
    |    b
    |  else
    |    c
    |})""" ==>
    """class A extends B(() => {
    |  if (a)
    |    b
    |  else
    |    c
    |})"""

  "class A { b => println(c) }" ==> "class A { b => println(c) }"

  "class A { @B val c = 4 }" ==> "class A { @B val c = 4 }"

  "trait A[@B C]" ==> "trait A[@B C]"

  "trait T[@specialized -A, @specialized +B]" ==> "trait T[@specialized -A, @specialized +B]"

  """class A { self: Int =>
    |println("foo")
    |}""" ==>
  """class A { self: Int =>
    |  println("foo")
    |}"""

  """class A {
    |  self: Int =>
    |  println("foo")
    |}""" ==>
  """class A {
    |  self: Int =>
    |  println("foo")
    |}"""

  "class A private[B](c: D)" ==> "class A private[B] (c: D)"

  "class A {\n  b()\n}" ==> "class A {\n  b()\n}"

  "class A {\r\n  b()\r\n}" ==> "class A {\r\n  b()\r\n}"

  """class A(
    |m: Int,
    |n: { def open(): Unit
    |def close(): Unit},
    |o: Int)""" ==>
  """class A(
    |  m: Int,
    |  n: {
    |    def open(): Unit
    |    def close(): Unit
    |  },
    |  o: Int)"""

  """class A(
    |n: Int, m: {def foo(): Int
    |def bar(a: String): Int}, o: Int,
    |p: Int,
    |m: {def foo(): Int
    |def bar(a: String): Int})""" ==>
  """class A(
    |  n: Int, m: {
    |    def foo(): Int
    |    def bar(a: String): Int
    |  }, o: Int,
    |  p: Int,
    |  m: {
    |    def foo(): Int
    |    def bar(a: String): Int
    |  })"""

{
   implicit val formattingPreferences = FormattingPreferences.
     setPreference(AlignParameters, true).
     setPreference(SpaceBeforeColon, true).
     setPreference(SpaceInsideBrackets, true)

  """def a(
    |a: Int = 1,
    |abc: Boolean = true): Int""" ==>
  """def a(
    |  a :   Int     = 1,
    |  abc : Boolean = true) : Int"""

  """def a(
    |a: Option[Either[Int]] = 1,
    |abc: Boolean = true): Int""" ==>
  """def a(
    |  a :   Option[ Either[ Int ] ] = 1,
    |  abc : Boolean                 = true) : Int"""
}

{
   implicit val formattingPreferences = FormattingPreferences.
     setPreference(AlignParameters, true).
     setPreference(RewriteArrowSymbols, true)

    // Formats rewritten arrows correctly
    """def A(
      |  a: A => B = null,
      |  bee: => B = null,
      |  c: B => C = null
      |): D""" ==>
    """def A(
      |  a:   A ⇒ B = null,
      |  bee: ⇒ B   = null,
      |  c:   B ⇒ C = null): D"""

  """class a(
     |  b: Int
     |)""" ==>
   """class a(
     |  b: Int)"""

   """class a(
     |  a: String = "",
     |  b: Int = 0
     |)(
     |  c: String = "",
     |  d: Int = 1
     |)(
     |  implicit
     |  val e: String = "",
     |  f: Int = 2
     |)""" =/=>
   """class a(
     |  a: String = ""
     |  b: Int    = 0
     |)(
     |  c: String = ""
     |  d: Int    = 1
     |)(
     |  implicit
     |  val e: String = ""
     |  f: Int        = 2
     |)"""
}

{
    implicit val formattingPreferences = FormattingPreferences.setPreference(AlignParameters, true)

    // Make sure spacing in same line implicit parameters is preserved
    """class A[T](a: T)(implicit b: B)""" ==>
      """class A[T](a: T)(implicit b: B)"""
    """class A[T](a: T)(implicit b: B, c: C)""" ==>
      """class A[T](a: T)(implicit b: B, c: C)"""
    """class A[T](a: T)(implicit b: B, c: C,
        |d: D)""" ==>
      """class A[T](a: T)(implicit b: B, c: C,
        |                 d: D)"""

   // Split into 3 columns: name, type, and default
  """def showInput[A](
    | parent: Component = null,
    | message: Any,
    | title: String = uiString("OptionPane.inputDialogTitle"),
    | messageType: Message.Value = Message.Question,
    | icon: Icon = EmptyIcon,
    | entries: Seq[A] = Nil,
    | initial: A): Option[A]""" ==>
  """def showInput[A](
    |  parent:      Component     = null,
    |  message:     Any,
    |  title:       String        = uiString("OptionPane.inputDialogTitle"),
    |  messageType: Message.Value = Message.Question,
    |  icon:        Icon          = EmptyIcon,
    |  entries:     Seq[A]        = Nil,
    |  initial:     A): Option[A]"""

    // Formats function types correctly
  """private def executeWithinClient[T](
    |crawlerConfig: String => JsValue = Fancy.function,
    |f: HttpCrawlerClient => T,
    |port: Int = SpecHelper.port
    |): T""" ==>
  """private def executeWithinClient[T](
    |  crawlerConfig: String => JsValue      = Fancy.function,
    |  f:             HttpCrawlerClient => T,
    |  port:          Int                    = SpecHelper.port): T"""

    // By name parameters have correct spacing
  """def a(
    |  p1: => (SomeLongByNameParam => SomeShorterParam) = Null,
    |  param2: SomeShorterParam = Null
    |): A""" ==>
  """def a(
    |  p1:     => (SomeLongByNameParam => SomeShorterParam) = Null,
    |  param2: SomeShorterParam                             = Null): A"""

    // Formats parameterized types correctly
    """def A(complicatedType: Option[B  ,C,      D[E, F,G]] = None,
      | simpleType: String = ""
      |): B""" ==>
    """def A(
      |  complicatedType: Option[B, C, D[E, F, G]] = None,
      |  simpleType:      String                   = ""): B"""

    // Param gets placed onto a new line due to current limitations of existing IntertokenFormatInstructions
  """case class Spacing(param: Int = 1,
    |paramTwo: Int = 2,
    |paramThree: String = "3"
    |)""" ==>
  """case class Spacing(
    |  param:      Int    = 1,
    |  paramTwo:   Int    = 2,
    |  paramThree: String = "3")"""

    // Groups and formats consecutive single line parameters (multi line params)
  """case class Spacing(param: Int = 1,
    |paramTwo: Int = 2,
    |paramThree: {
    |  val test: Int
    |},
    |paramFour: Option[String] = Some("One"),
    |paramFive: Any = Nothing)""" ==>
  """case class Spacing(
    |  param:    Int = 1,
    |  paramTwo: Int = 2,
    |  paramThree: {
    |    val test: Int
    |  },
    |  paramFour: Option[String] = Some("One"),
    |  paramFive: Any            = Nothing)"""

    // Groups and formats consecutive single line parameters (newlines)
  """case class Spacing(
    |param: Int = 1,
    |paramTwo: Int = 2,
    |
    |paramFour: Option[String] = Some("One"),
    |paramFive: Any = Nothing
    |)""" ==>
  """case class Spacing(
    |  param:    Int = 1,
    |  paramTwo: Int = 2,
    |
    |  paramFour: Option[String] = Some("One"),
    |  paramFive: Any            = Nothing)"""

    // Aligns implicits and curried parameters properly
  """class SomeClass(
    |parameterOne: Int = 1,
    |val parameterTwo: Option[String] = None,
    |three: String = "three"
    |)(
    |intermediate: Int
    |)(
    |implicit val four: Int,
    |five: String,
    |six: Boolean)""" ==>
  """class SomeClass(
    |  parameterOne:     Int            = 1,
    |  val parameterTwo: Option[String] = None,
    |  three:            String         = "three")(
    |  intermediate: Int)(
    |  implicit
    |  val four: Int,
    |  five:     String,
    |  six:      Boolean)"""

   // Handles annotations, modifiers, and comments
  """def extraStuff(
    |// comment 1
    |@Annotated paramOne: Int = 1, // comment 2
    |/* comment 3 */ private val modifiedTwo: String = "two",
    |@Annotated2("complicatedAnnotation") @A3("Another") protected annotatedAndModified: Option[Int] = Some(3))""" ==>
  """def extraStuff(
    |  // comment 1
    |  @Annotated paramOne:                                                               Int         = 1, // comment 2
    |  /* comment 3 */ private val modifiedTwo:                                           String      = "two",
    |  @Annotated2("complicatedAnnotation")@A3("Another") protected annotatedAndModified: Option[Int] = Some(3))"""

  """class A(n: Int,
    |z: { val m
    |val n },
    |m: Int)""" ==>
  """class A(
    |  n: Int,
    |  z: {
    |    val m
    |    val n
    |  },
    |  m: Int)"""

  """class A(m: Int,
    |n: {
    |val x: String
    |val y : String
    |},
    | o: Int = {
    |  42
    |})""" ==>
  """class A(
    |  m: Int,
    |  n: {
    |    val x: String
    |    val y: String
    |  },
    |  o: Int = {
    |    42
    |  })"""

  """class A(n: {
    | def close(): Unit
    | def open(): Unit
    |},
    |m: Int)""" ==>
  """class A(
    |  n: {
    |    def close(): Unit
    |    def open(): Unit
    |  },
    |  m: Int)"""

  """class A(
    |implicit n: {
    |def x: Int
    |def y: Int
    |})""" ==>
  """class A(
    |  implicit
    |  n: {
    |    def x: Int
    |    def y: Int
    |  })"""

  """class A(n: {
    |def x: Int
    |})""" ==>
  """class A(n: {
    |          def x: Int
    |        })"""

  """class A(a: Int,
    |b: Int)(c: { val d: Int
    |})""" ==>
  """class A(
    |  a: Int,
    |  b: Int)(c: {
    |            val d: Int
    |          })"""

  }

  """class A(a: Int,
    |b: Int)(c: { val d: Int
    |})""" ==>
  """class A(
    |  a: Int,
    |  b: Int)(c: {
    |  val d: Int
    |})"""

  {
    implicit val formattingPreferences = FormattingPreferences.setPreference(DoubleIndentClassDeclaration, true)
    """class Person(
      |  name: String,
      |  age: Int)
      |    extends Entity
      |    with Logging
      |    with Identifiable
      |    with Serializable""" ==>
    """class Person(
      |  name: String,
      |  age: Int)
      |    extends Entity
      |    with Logging
      |    with Identifiable
      |    with Serializable"""

    """class Person(
      |    name: String,
      |    age: Int) {
      |  def firstMethod = 42
      |}""" ==>
    """class Person(
      |  name: String,
      |  age: Int) {
      |  def firstMethod = 42
      |}"""

    // Test that first arguments are put on the next line correctly.
    """class Person(name: String,
      |    age: Int) {
      |  def firstMethod = 42
      |}""" ==>
    """class Person(
      |  name: String,
      |  age: Int) {
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
    |    extends Entity
    |    with Logging
    |    with Identifiable
    |    with Serializable {
    |  def firstMethod = 42
    |}"""

    """class Person(
      |name: String,
      |  age: Int)
      |extends Entity  {
      |def method() = 42
      |}""" ==>
    """class Person(
      |  name: String,
      |  age: Int)
      |    extends Entity {
      |  def method() = 42
      |}"""

  """trait A
    |extends B
    |with C {
    |println("d")
    |}""" ==>
  """trait A
    |    extends B
    |    with C {
    |  println("d")
    |}"""

  }

  "trait Function1[@specialized(Int, Long, Double) -T1, @specialized(Unit, Int, Long, Double) +R] extends AnyRef" ==>
  "trait Function1[@specialized(Int, Long, Double) -T1, @specialized(Unit, Int, Long, Double) +R] extends AnyRef"

"""class C
    |extends {
    |val name = "Bob"
    |}""" ==>
  """class C
    |  extends {
    |    val name = "Bob"
    |  }"""

  """class A // foo
    |/*bar*/
    |{}""" ==>
  """class A // foo
    |/*bar*/ {}"""

  "private [a] trait B" ==> "private[a] trait B"
  "private [a] class B" ==> "private[a] class B"
  "private [a] case class B" ==> "private[a] case class B"
  "private [a] object B" ==> "private[a] object B"

  """private [a]
    |trait b""" ==>
  """private[a] trait b"""

  """class A {
    |
    |}""" ==>
  """class A {
    |
    |}"""

  """class A {
    |  val a = <xml:unparsed>&<<>""^%@$!#</xml:unparsed>
    |}""" ==>
  """class A {
    |  val a = <xml:unparsed>&<<>""^%@$!#</xml:unparsed>
    |}"""

  """class A(b: (_ <: C, _ <: D))""" ==>
  """class A(b: (_ <: C, _ <: D))"""

  """class A {
    |   def b() {
    |     case class C
    |   }
    |}""" ==>
  """class A {
    |  def b() {
    |    case class C
    |  }
    |}"""

  """class A {
    |  trait B <: { val x: Int = 3 }
    |}""" ==>
  """class A {
    |  trait B <: { val x: Int = 3 }
    |}"""

  """class A  { b =>
    |  c
    |  d
    |}""" ==>
  """class A { b =>
    |  c
    |  d
    |}"""

  """class A1 private[B]
    |()""" ==>
  """class A1 private[B] ()"""

  "class A @Deprecated ()" ==> "class A @Deprecated()"

  """class A @B ()
    |()""" ==>
  """class A @B() ()"""

  // See Scala trac #3672 for next two cases:

  """object test {
    |  val f: (Int => Int)  = { implicit x => x }
    |  val g: (Int => Int)  = implicit x => x
    |  val h: (Int => Int)  = {(); implicit x => x}
    |}""" ==>
  """object test {
    |  val f: (Int => Int) = { implicit x => x }
    |  val g: (Int => Int) = implicit x => x
    |  val h: (Int => Int) = { (); implicit x => x }
    |}"""

  """object Test {
    |  def foo(f: Int => Int) = () ; foo { implicit x : Int => x + 1 }
    |  def bar(f: Int => Int) = () ; foo { x : Int => x + 1 }
    |}""" ==>
  """object Test {
    |  def foo(f: Int => Int) = (); foo { implicit x: Int => x + 1 }
    |  def bar(f: Int => Int) = (); foo { x: Int => x + 1 }
    |}"""

  """class A {
    |  @b // c
    |   val d
    |}""" ==>
  """class A {
    |  @b // c
    |  val d
    |}"""

  """class A {
    |@b /* */
    |def c
    |}""" ==>
  """class A {
    |  @b /* */
    |  def c
    |}"""

  "@foo\r\ntrait X" ==> "@foo\r\ntrait X"

  """object A {
    |  @B
    |  @C
    |  @D def e
    |}""" ==>
  """object A {
    |  @B
    |  @C
    |  @D def e
    |}"""

  """class A extends B(42 , 43) with C(44 , 45) {
    |  foo()
    |  bar()
    |}""" ==>
  """class A extends B(42, 43) with C(44, 45) {
    |  foo()
    |  bar()
    |}"""

//  format: ON

  override val debug = false

  def parse(parser: ScalaParser) = parser.nonLocalDefOrDcl()

  type Result = FullDefOrDcl

  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState(indentLevel = 0))

}
