package scalariform.formatter

import scalariform.parser._

// format: OFF
class TypeFormatterTest extends AbstractFormatterTest {

  "Int" ==> "Int"
  "List [ String ] " ==> "List[String]"
  "Map[String,List[ Int]]" ==> "Map[String, List[Int]]"
 
  "A => B" ==> "A => B"  

  "List/*b*/[/*c*/String/*d*/]" ==> "List /*b*/ [ /*c*/ String /*d*/ ]"
  
  "Int /*foo*/ Either  String" ==> "Int /*foo*/ Either String"
  
  "List[_>:A<:B]" ==> "List[_ >: A <: B]"

  "List[_>:A_ <:B_]" ==> "List[_ >: A_ <: B_]"
  "A_ @Deprecated" ==> "A_ @Deprecated"
  "A Either B @Deprecated" ==> "A Either B @Deprecated" 
  
  "A#B" ==> "A#B"
  "A_ #B" ==> "A_ #B"
  "A_# #B" ==> "A_# #B"

  "this . type" ==> "this.type" 
  
  "(A)#X" ==> "(A)#X"

  "A @cps[A, C]" ==> "A @cps[A, C]"
  
  "Int @cps[Int,Int]" ==> "Int @cps[Int, Int]"

  "{def bar :Unit}" ==> "{ def bar: Unit }"

  "(A, B) => C" ==> "(A, B) => C"

  "(A*) => B" ==> "(A*) => B" 
  
  "(=> A) => B" ==> "(=> A) => B" 

  "(C, => A) => B" ==> "(C, => A) => B"

  "(C, A*) => B" ==> "(C, A*) => B"

  "(A*, B) => C" ==> "(A*, B) => C"

  "(=>A)" ==> "(=> A)"
  "(=> A#Inner[Int])" ==> "(=> A#Inner[Int])"
  
  "Int Either String" ==> "Int Either String"

  // TODO: forSome clause not valid 
  "(=> A#Inner[Int] @Deprecated with B with (C) @Deprecated Either (B, A#Inner[_ <: B]) with C) => B forSome {}" ==>
  "(=> A#Inner[Int] @Deprecated with B with (C) @Deprecated Either (B, A#Inner[_ <: B]) with C) => B forSome {}"
   
  "(=> A with B Either (B, A)) => B" ==>
  "(=> A with B Either (B, A)) => B"
  
  "b[c# ::[d]]" ==> "b[c# ::[d]]"

  override val debug = false
  
  type Result = Type

  def parse(parser: ScalaParser) = parser.typ() // TODO: ensure EOF
  
  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState(indentLevel = 0))
  
}
