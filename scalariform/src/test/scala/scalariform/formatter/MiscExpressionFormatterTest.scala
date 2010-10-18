package scalariform.formatter

import scalariform.parser._
import scalariform.formatter._
import scalariform.formatter.preferences._

// format: OFF
class MiscExpressionFormatterTest extends AbstractExpressionFormatterTest {

  override val debug = false

  "→" ==> "→"
  "this" ==> "this"
  "super [ B ] . t" ==> "super[B].t"
  "E2 . super [ B ] . t" ==> "E2.super[B].t"
  "1" ==> "1"
  "true" ==> "true"
  "2.345" ==> "2.345"
  "2 + 2" ==> "2 + 2"

  "a max(b)" ==> "a max (b)"
  "-5f max(2)" ==> "-5f max (2)"
  "-5 max(2)" ==> "-5 max (2)"
  "-5f.max(2)" ==> "-5f.max(2)"
  "-5.max(2)" ==> "-5.max(2)"
 
  "42" ==> "42"
  "-42" ==> "-42"

  """println("hello")""" ==> """println("hello")"""
  "1 * (2 + 3) * 4" ==> "1 * (2 + 3) * 4"
  """println(getClass().getSimpleName() + " passed.")""" ==> """println(getClass().getSimpleName() + " passed.")"""

  "a(b).c" ==> "a(b).c"

  "b = 2" ==> "b = 2"
  "b_+= = 2" ==> "b_+= = 2"
  "2: Int" ==> "2: Int"
  "x: _*" ==> "x: _*"
  "x: _ *" ==> "x: _*"
  "x_ : Int" ==> "x_ : Int"
  "|v| : Int" ==> "|v| : Int"
  "×× : (A, A)" ==> "×× : (A, A)"
  
  "{ case (_~_) => }" ==> "{ case (_ ~ _) => }"
  "{ case (a~_) => }" ==> "{ case (a ~ _) => }"
  "{ case (_~b) => }" ==> "{ case (_ ~ b) => }"

  "(1, 2, 3, 4)" ==> "(1, 2, 3, 4)"
  "(x: Int, y: List[String], z: Map[_ <: K, V])" ==> "(x: Int, y: List[String], z: Map[_ <: K, V])"

  "a(3)=5" ==> "a(3) = 5"

  "-3 + -2" ==> "-3 + -2"
  "-3+(-2)" ==> "-3 + (-2)"
  "-a * +b * ~c * !d" ==> "-a * +b * ~c * !d"
  "FOO?" ==> "FOO?"
  "x_ ?" ==> "x_ ?"
  "palindrome reverse" ==> "palindrome reverse"

  "_" ==> "_"
  "println _" ==> "println _"
  "s.isInstanceOf[String]" ==> "s.isInstanceOf[String]"

  "placeholderParams = treeCopy.ValDef(vd, mods, name, tpt.duplicate, EmptyTree) :: rest" ==>
  "placeholderParams = treeCopy.ValDef(vd, mods, name, tpt.duplicate, EmptyTree) :: rest"

  "in.token == ARROW && (location != InTemplate || lhsIsTypedParamList)" ==>
  "in.token == ARROW && (location != InTemplate || lhsIsTypedParamList)"

  "stripParens(reduceStack(true, base, topinfo.operand, 0, true))" ==>
  "stripParens(reduceStack(true, base, topinfo.operand, 0, true))"

  "surround(open, close)(enumerators(), Nil)" ==> "surround(open, close)(enumerators(), Nil)"

  "makeColour(red = 253, green = 712, blue = 120)" ==> "makeColour(red = 253, green = 712, blue = 120)"

  "_ => 3" ==> "_ => 3"
  "(_: Int) => 3" ==> "(_: Int) => 3"
  "(x: String, y: Map[String, String]) => y(x)" ==> "(x: String, y: Map[String, String]) => y(x)"

  """2: @Foo({println("bar")
    |3})""" ==> 
  """2: @Foo({
    |  println("bar")
    |  3
    |})"""

  """f: { def n: Int
    |def m: Int}""" ==>
  """f: {
    |  def n: Int
    |  def m: Int
    |}"""
    
    
  """List[Int { val n: Int
    |val m: Int }]()""" ==>
  """List[Int {
    |  val n: Int
    |  val m: Int
    |}]()"""

    
  """42 match {
    |case x: Int { val n: Int
    |val m: Int } => 42
    |}""" ==>
  """42 match {
    |  case x: Int {
    |    val n: Int
    |    val m: Int
    |  } => 42
    |}"""
    
  """42 match  {
    |  case foo_ @Bar =>
    |}""" ==>
  """42 match {
    |  case foo_ @Bar =>
    |}"""    
    
  "NEWLINE" ==> "NEWLINE"
  "NEWLINES" ==> "NEWLINES"

  "\"\"\"triplequoted\"\"\"" ==> "\"\"\"triplequoted\"\"\""

  "{ type x = Equal[App[Lam[X], X]#Eval, X] }" ==> "{ type x = Equal[App[Lam[X], X]#Eval, X] }"

  """{ 
    |  type U = Int
    |  type T <: Seq[U] 
    |}""" ==>
  """{
    |  type U = Int
    |  type T <: Seq[U]
    |}"""

  "{ type X[+T]=List[T]}" ==> "{ type X[+T] = List[T] }"

  "new A(b, c)" ==> "new A(b, c)"

  "1/2" ==> "1 / 2"
  "1+/2" ==> "1 +/ 2"
  "1+/+2" ==> "1 +/+ 2"
  "1*/2" ==> "1 */ 2"

  "1*/*a*/2" ==> "1 * /*a*/ 2"
  "1 +/* /* var */ var */2" ==> "1 + /* /* var */ var */ 2"
  "1 + /* /* var */ var */2" ==> "1 + /* /* var */ var */ 2"

  """(1//2
    |+2)""" ==>
  """(1 //2
    |  + 2)""" // TODO: Review
    
  "1.to(13)" ==> "1.to(13)"
  "0.asInstanceOf[Character]" ==> "0.asInstanceOf[Character]"

  "b match { case _: List[List[C]] => d }" ==> "b match { case _: List[List[C]] => d }"

  """for {
    |  n <- Some(42)
    |  _ <- Some(42)
    |} yield n""" ==>
  """for {
    |  n <- Some(42)
    |  _ <- Some(42)
    |} yield n"""

  "foo(implicit x => implicitly[X])" ==> "foo(implicit x => implicitly[X])"

  "(x: Int) => 42" ==> "(x: Int) => 42"

  "tail.partition(_<pivot)" ==> "tail.partition(_ < pivot)"

  "postWorkItem { () => }" ==> "postWorkItem { () => }"

  """new C(new D {
    |println("foo")
    |println("bar")
    |})""" ==>
  """new C(new D {
    |  println("foo")
    |  println("bar")
    |})"""

  """b match { 
    |case y@ <phone/> => 
    |}""" ==>
  """b match {
    |  case y@ <phone/> =>
    |}""" // TODO: Whitespace around @ in this case?


  """1 / // foo
    |2""" ==>
  """1 / // foo
    |  2"""
  
  "(USCORE ~ opt(wildcardType) |/ /* id |/ */typ_)" ==> "(USCORE ~ opt(wildcardType) |/ /* id |/ */ typ_)" 

  "a match {case b => c; case d => e}" ==> "a match { case b => c; case d => e }"
  
  "Option(foo) match { case Some(x)=> 42 case None => 12}" ==> "Option(foo) match { case Some(x) => 42 case None => 12 }"
    
  """a match { 
    |case b|(c) => 
    |}""" ==>
  """a match {
    |  case b | (c) =>
    |}"""
  
  """new B
    |{
    |println("foo")
    |}""" ==>
  """new B {
    |  println("foo")
    |}"""

  """println
    |{foo
    |}""" ==>
  """println {
    |  foo
    |}""" 
    
  """println
    |{
    | foo}""" ==>
  """println {
    |  foo
    |}"""
    
  """println
    |{foo}""" ==>
  """println { foo }"""
    
    
  """doBlock(xs) {(x:Int) => println(x)
    |println("bobble")
    |}""" ==>
  """doBlock(xs) { (x: Int) =>
    |  println(x)
    |  println("bobble")
    |}"""

  """doBlock(xs) {x => println(x)
    |println("bobble")
    |}""" ==>
  """doBlock(xs) { x =>
    |  println(x)
    |  println("bobble")
    |}"""
    
  """doBlock(xs) {(x:Int) => println(x)}""" ==> """doBlock(xs) { (x: Int) => println(x) }"""

  """foo
    |{ bar }
    |{ baz }""" ==>
  """foo { bar } { baz }"""

  """foo { bar } { baz }""" ==> """foo { bar } { baz }"""

  """foo { () => 
    |}""" ==>
  """foo { () =>
    |}"""    

  """3 +// foo
    |4""" ==>
  """3 + // foo
    |  4"""

  """3 +
    |{ 4 * 12
    |}""" ==>
  """3 +
    |  {
    |    4 * 12
    |  }"""

  """1 + 
    |2 + 
    |3""" ==>
  """1 +
    |  2 +
    |  3"""

  """foo(1,
    |2)""" ==>
  """foo(1,
    |  2)"""

  """/* a */
    |b""" ==>
  """/* a */ b"""

  """a(
    |if (b) c)""" ==>
  """a(
    |  if (b) c)"""

  """a(
    |if (b)
    |c)""" ==>
  """a(
    |  if (b)
    |    c)"""

  """a("A", 
    |  b("B",
    |     c(1, 2),
    |     c(3, 4)), 
    |  b("B2", 
    |     c(5, 6)))""" ==>
  """a("A",
    |  b("B",
    |    c(1, 2),
    |    c(3, 4)),
    |  b("B2",
    |    c(5, 6)))"""

  """1 + (a,
    | b, c)""" ==>
  """1 + (a,
    |  b, c)"""

  """1 + (
    |  a, b, c)""" ==>
  """1 + (a, b, c)"""

  """1 + (a
    |, b, c)""" ==>
  """1 + (a, b, c)"""

  "()" ==> "()"

  {
    implicit val formattingPreferences = FormattingPreferences.setPreference(SpaceBeforeColon, true)
    "(a: Int) => 3" ==> "(a : Int) => 3"
  }
  """{ // format: +spaceBeforeColon
    |  val a:Int = 2
    |}""" ==>   
  """{ // format: +spaceBeforeColon
    |  val a : Int = 2
    |}""" 

  "_.a" ==> "_.a"

  {
    implicit val formattingPreferences = FormattingPreferences.setPreference(CompactStringConcatenation, true)
    """"foo"+"bar"""" ==> """"foo"+"bar""""
    """"foo" + "bar"""" ==> """"foo"+"bar""""
    """foo + "bar"""" ==> """foo+"bar""""
    """"foo" + bar""" ==> """"foo"+bar"""
    """foo + bar""" ==> """foo + bar"""
  }

  """a { implicit b =>
    |println(b)
    |}""" ==>
  """a { implicit b =>
    |  println(b)
    |}"""

  """a { b =>
    |println(b)
    |}""" ==>
  """a { b =>
    |  println(b)
    |}"""

  """a(b,
    |/* c */d)""" ==>
  """a(b,
    |  /* c */ d)"""

  """submit(
    |
    |)""" ==>
  """submit()"""

  """(
    |42, 
    |46
    |)""" ==>
  """(42,
    |  46)""" // I prefer no initial indent for tuples, although you could argue it should be consistent with ParenExprs
  
  """a(b,
    |c => {
    |d})""" ==>
  """a(b,
    |  c => {
    |    d
    |  })"""

  """a(b, 
    |(c), {
    |d})""" ==>
  """a(b,
    |  (c), {
    |    d
    |  })"""

  """a(
    |    () => 
    |    b)""" ==>
  """a(
    |  () =>
    |    b)"""

 """foobar(
    |(1,2),
    |(3, 4), 
    |(5, 6), 
    |(7, 8))""" ==>
  """foobar(
    |  (1, 2),
    |  (3, 4),
    |  (5, 6),
    |  (7, 8))"""

  """(1
    |,2)""" ==>
  """(1, 2)"""

  """a(1
    |,2)""" ==>
  """a(1, 2)"""

  """42
    |: Int""" ==>
  """42: Int"""

  "if (true) 1; else 2" ==> "if (true) 1; else 2" // Check SEMI + ELSE rule

  "a: ::" ==> "a: ::"

}
